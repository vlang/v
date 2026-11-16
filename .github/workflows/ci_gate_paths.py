#!/usr/bin/env python3
"""Preserve master-push path filters when CI suites are called after preflight.

GitHub's workflow_call trigger cannot have a paths filter. The manifests store
the original, ordered on.push paths/paths-ignore patterns. The caller runs this
script against the push's exact before/after commits after Linux bootstrap and fmt.
No external Python packages are required.
"""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(".github/workflows")
MANIFEST_DIR = ROOT / "ci_gate_filters"
CALLER = ROOT / "ci_master.yml"
SHARED_COMPILER_FILES = {
    ".github/workflows/bootstrapping_ci.yml",
    ".github/workflows/prepare_v.sh",
    ".github/workflows/prepare_v.ps1",
    "cmd/tools/build_vprod.sh",
}


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
        if entry["shard"] != "master":
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
        if path.name in known or path == CALLER:
            continue
        if re.search(r"(?m)^  push:\s*$", path.read_text(encoding="utf-8")):
            raise ValueError(f"ungated master-push workflow: {path.name}")
    # GitHub counts both bootstrap workflows and all called suites.
    if sum(not entry.get("inline") for entry in entries) + 2 > 50:
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
        print("No usable push base; conservatively run all suites", file=sys.stderr)
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


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("shard", choices=("master", "validate"))
    args = parser.parse_args()
    entries = load_entries()
    validate(entries)
    if args.shard == "validate":
        print(f"Validated {len(entries)} gated push suites")
        return
    changed = changed_paths(os.environ.get("CI_BEFORE"), os.environ.get("CI_AFTER"))
    selected = entries
    output_path = os.environ.get("GITHUB_OUTPUT")
    lines = []
    for entry in selected:
        enabled = should_run(changed, entry)
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
