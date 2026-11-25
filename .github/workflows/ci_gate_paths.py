#!/usr/bin/env python3
"""Preserve master-push path filters when CI suites are called after preflight.

GitHub's workflow_call trigger cannot have a paths filter. The manifests store
the original, ordered on.push paths/paths-ignore patterns. The caller runs this
script in each master-push lane. It selects suites from the changes since that
lane's newest finished run whose suites started. A newer push can replace a
waiting run, so a later run must cover the commits it skipped.
No external Python packages are required.
"""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

ROOT = Path(".github/workflows")
MANIFEST_DIR = ROOT / "ci_gate_filters"
CALLER = ROOT / "ci_master.yml"
LANE_WORKFLOWS = {"master": CALLER, "portability": ROOT / "ci_master_portability.yml"}
SHARED_COMPILER_FILES = {
    ".github/workflows/bootstrapping_ci.yml",
    ".github/workflows/prepare_v.sh",
    ".github/workflows/prepare_v.ps1",
    "cmd/tools/build_vprod.sh",
}
# Uploaded by record-tested-commit once all bootstrap and VC gates for the lane pass.
TESTED_MARK = "ci-tested-commit"
# Conclusions of runs whose started suites all ran to the end.
FINISHED = {"success", "failure"}
MAX_RUNS = 50


def glob_pattern(pattern):
    """Translate the * and ** portions of GitHub path globs to a regex."""
    parts = ["^"]
    i = 0
    while i < len(pattern):
        char = pattern[i]
        if char == "*":
            if pattern[i:i + 2] == "**":
                if pattern[i + 2:i + 3] == "/":
                    parts.append("(?:.*/)?")
                    i += 3
                else:
                    parts.append(".*")
                    i += 2
            else:
                parts.append("[^/]*")
                i += 1
        elif char == "?":
            parts.append("[^/]")
            i += 1
        elif char == "[":
            end = pattern.find("]", i + 1)
            if end != -1:
                body = pattern[i + 1:end]
                if body.startswith("!"):
                    body = "^" + body[1:]
                parts.append("[" + body + "]")
                i = end + 1
            else:
                parts.append(r"\[")
                i += 1
        else:
            parts.append(re.escape(char))
            i += 1
    return re.compile("".join(parts) + "$")


def path_is_selected(path, mode, patterns):
    if mode == "all":
        return True
    selected = mode == "paths-ignore"
    for pattern in patterns:
        exclude = pattern.startswith("!")
        raw = pattern[1:] if exclude else pattern
        if glob_pattern(raw).fullmatch(path):
            selected = exclude if mode == "paths-ignore" else not exclude
    return selected


def should_run(changed, entry):
    return changed is None or any(
        path in SHARED_COMPILER_FILES or path.startswith(".github/actions/setup-v/")
        for path in changed
    ) or entry["mode"] == "all" or any(
        path_is_selected(path, entry["mode"], entry["patterns"]) for path in changed
    )


def should_run_in_lane(changed, entry, lane):
    return (entry["shard"] == lane or entry["workflow"] == "gen_vc_ci.yml") and should_run(
        changed, entry
    )


def load_entries():
    entries = []
    for file in sorted(MANIFEST_DIR.glob("*.json")):
        entries.extend(json.loads(file.read_text(encoding="utf-8")))
    names = [entry["workflow"] for entry in entries]
    if len(names) != len(set(names)):
        raise ValueError("duplicate workflow in CI gate manifests")
    return entries


def validate(entries):
    """Catch direct-push bypasses, missing inlined tests and call-limit overflows."""
    if not entries:
        raise ValueError("CI gate manifests are empty")
    known = {entry["workflow"] for entry in entries}
    caller = CALLER.read_text(encoding="utf-8")
    for entry in entries:
        name = entry["workflow"]
        if entry["shard"] not in LANE_WORKFLOWS:
            raise ValueError(f"unknown shard: {name}")
        if entry["mode"] not in ("all", "paths", "paths-ignore"):
            raise ValueError(f"unknown filter mode: {name}")
        source_path = ROOT / name
        key = Path(name).stem
        if entry.get("inline"):
            if source_path.exists():
                raise ValueError(f"inlined workflow must not remain active: {name}")
            if not re.search(rf"(?m)^  inline_{re.escape(key)}:$", caller):
                raise ValueError(f"{name} has no inlined gated job")
        else:
            if not source_path.exists():
                raise ValueError(f"missing reusable workflow: {name}")
            source = source_path.read_text(encoding="utf-8")
            if not re.search(r"(?m)^  workflow_call:\s*$", source):
                raise ValueError(f"{name} must expose workflow_call")
            if f"uses: ./.github/workflows/{name}" not in caller:
                raise ValueError(f"{name} missing from master CI caller")
            if not re.search(rf"(?m)^  {re.escape(key)}:$", caller):
                raise ValueError(f"{name} missing a caller job")
        if f"needs.preflight.outputs.{key} == 'true'" not in caller:
            raise ValueError(f"{name} can bypass preflight")
    for path in ROOT.glob("*.yml"):
        if path.name in known or path in LANE_WORKFLOWS.values():
            continue
        if re.search(r"(?m)^  push:\s*$", path.read_text(encoding="utf-8")):
            raise ValueError(f"ungated master-push workflow: {path.name}")
    # The portability caller adds the shared gate to both bootstrap workflows
    # and the called suites.
    if sum(not entry.get("inline") for entry in entries) + 3 > 50:
        raise ValueError("too many unique reusable workflows (>50)")
    assert path_is_selected("cmd/tools/builders/build.v", "paths-ignore",
                            ["cmd/tools/**", "!cmd/tools/builders/**.v"])
    assert not path_is_selected("cmd/tools/oldv.v", "paths-ignore",
                                ["cmd/tools/**", "!cmd/tools/builders/**.v"])
    assert path_is_selected("vlib/net/http/h2spec/test.v", "paths",
                            ["vlib/net/http/**", "!**.md"])
    assert not path_is_selected("vlib/net/http/readme.md", "paths",
                                ["vlib/net/http/**", "!**.md"])


def changed_paths(before, after):
    if not before or not after or set(before) == {"0"}:
        print("No tested base commit; conservatively run all suites", file=sys.stderr)
        return None
    try:
        result = subprocess.run(
            ["git", "diff", "--name-only", "-z", before, after, "--"],
            check=True, capture_output=True,
        )
        names = [x.decode("utf-8") for x in result.stdout.split(b"\0") if x]
        # Mirrors GitHub's >1000-commit safety rule (run rather than skip).
        count = subprocess.run(
            ["git", "rev-list", "--count", f"{before}..{after}"],
            check=True, capture_output=True, text=True,
        )
        if int(count.stdout.strip()) > 1000:
            return None
        return names
    except (OSError, subprocess.CalledProcessError, ValueError) as exc:
        print(f"Cannot determine changed paths ({exc}); run all", file=sys.stderr)
        return None


class GitHubAPI:
    def __init__(self, token, repository):
        if not token or not repository:
            raise ValueError("GH_TOKEN and GITHUB_REPOSITORY are required")
        self.token = token
        self.repository = repository

    def get(self, path):
        request = Request(
            f"https://api.github.com/repos/{self.repository}/{path}",
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self.token}",
                "X-GitHub-Api-Version": "2022-11-28",
                "User-Agent": "vlang-ci-gate-paths",
            },
        )
        last_error = None
        for _ in range(3):
            try:
                with urlopen(request, timeout=30) as response:
                    return json.load(response)
            except (HTTPError, URLError, OSError, ValueError) as error:
                last_error = error
        raise RuntimeError(f"GitHub API GET {path} failed: {last_error}")


def is_ancestor(commit, after):
    return subprocess.run(
        ["git", "merge-base", "--is-ancestor", commit, after], capture_output=True,
    ).returncode == 0


def tested_base(api, after, run_id, lane, ancestor=is_ancestor):
    """Return the head of the newest finished run in the lane that started its suites."""
    runs = api.get(
        f"actions/workflows/{LANE_WORKFLOWS[lane].name}/runs"
        f"?branch=master&event=push&status=completed&per_page={MAX_RUNS}"
    ).get("workflow_runs")
    if not isinstance(runs, list):
        raise ValueError("invalid workflow run listing")
    for run in runs:
        sha = run.get("head_sha")
        if (run.get("id") == run_id or run.get("conclusion") not in FINISHED
                or not isinstance(sha, str) or sha == after or not ancestor(sha, after)):
            continue
        artifacts = api.get(
            f"actions/runs/{run['id']}/artifacts?name={TESTED_MARK}"
        ).get("artifacts")
        if any(a.get("name") == TESTED_MARK and not a.get("expired")
               for a in artifacts or []):
            print(f"Selecting suites from changes since run {run['id']} at {sha}",
                  file=sys.stderr)
            return sha
    print(f"No finished run among the newest {MAX_RUNS} marked its suites as started",
          file=sys.stderr)
    return None


def main_base(after, lane):
    try:
        api = GitHubAPI(os.environ.get("GH_TOKEN"), os.environ.get("GITHUB_REPOSITORY"))
        return tested_base(api, after, int(os.environ.get("GITHUB_RUN_ID") or 0), lane)
    except (RuntimeError, ValueError, KeyError, AttributeError, TypeError) as exc:
        print(f"Cannot find the last tested commit ({exc})", file=sys.stderr)
        return None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("shard", choices=("master", "validate"))
    args = parser.parse_args()
    entries = load_entries()
    validate(entries)
    if args.shard == "validate":
        print(f"Validated {len(entries)} gated push suites")
        return
    after = os.environ.get("CI_AFTER")
    lane = os.environ.get("CI_LANE", "master")
    if lane not in LANE_WORKFLOWS:
        parser.error(f"unknown CI_LANE: {lane}")
    changed = changed_paths(main_base(after, lane) if after else None, after)
    output_path = os.environ.get("GITHUB_OUTPUT")
    lines = []
    for entry in entries:
        enabled = should_run_in_lane(changed, entry, lane)
        key = Path(entry["workflow"]).stem
        lines.append(f"{key}={'true' if enabled else 'false'}")
        print(f"{entry['workflow']}: {'run' if enabled else 'skip'}")
    if output_path:
        with open(output_path, "a", encoding="utf-8") as output:
            output.write("\n".join(lines) + "\n")
    else:
        print("\n".join(lines))


if __name__ == "__main__":
    main()
