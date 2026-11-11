#!/usr/bin/env python3
"""Remove obsolete compiler artifacts after a replacement bootstrap succeeds."""
import json
import os
import re
import sys
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

REPOSITORY = "vlang/v"
WORKFLOW = ".github/workflows/ci_master.yml"
PLATFORMS = {
    "linux-x64", "macos-arm64", "macos-x64", "windows-x64", "termux-arm64", "termux-x64",
}
ARTIFACT_NAME = re.compile(
    r"vprod-(linux-x64|macos-arm64|macos-x64|windows-x64|termux-arm64|termux-x64)-([0-9a-f]{40})"
)
COMMIT = re.compile(r"[0-9a-f]{40}")
PAGE_SIZE = 100
MAX_PAGES = 100
API_ROOT = f"repos/{REPOSITORY}/actions"


class APIError(RuntimeError):
    def __init__(self, method, path, status):
        self.status = status
        super().__init__(f"GitHub API {method} {path} returned HTTP {status}")


class GitHubAPI:
    def __init__(self, token):
        if not token:
            raise ValueError("GH_TOKEN is required")
        self.token = token

    def request(self, method, path):
        request = Request(
            f"https://api.github.com/{path}",
            method=method,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self.token}",
                "X-GitHub-Api-Version": "2022-11-28",
                "User-Agent": "vlang-vprod-cleanup",
            },
        )
        try:
            with urlopen(request, timeout=30) as response:
                return json.load(response) if method == "GET" else None
        except HTTPError as error:
            raise APIError(method, path, error.code) from error
        except (URLError, OSError, ValueError) as error:
            raise RuntimeError(f"GitHub API {method} {path} failed") from error


def positive_id(value):
    return isinstance(value, int) and not isinstance(value, bool) and value > 0


def list_artifacts(api, path):
    """Snapshot every page before deleting anything or offsets would shift."""
    artifacts = []
    for page in range(1, MAX_PAGES + 1):
        response = api.request("GET", f"{path}?per_page={PAGE_SIZE}&page={page}")
        if not isinstance(response, dict) or not isinstance(response.get("artifacts"), list):
            raise ValueError(f"Invalid artifact listing: {path}")
        items = response["artifacts"]
        artifacts.extend(items)
        if len(items) < PAGE_SIZE:
            return artifacts
    raise ValueError(f"Artifact listing exceeded {MAX_PAGES} pages; leaving artifacts intact")


def trusted_run(run):
    if not isinstance(run, dict):
        return False
    repository = run.get("repository") or {}
    head_repository = run.get("head_repository") or {}
    if not isinstance(repository, dict) or not isinstance(head_repository, dict):
        return False
    return (
        repository.get("full_name") == REPOSITORY
        and head_repository.get("full_name") == REPOSITORY
        and positive_id(repository.get("id"))
        and positive_id(head_repository.get("id"))
        and run.get("path") == WORKFLOW
        and run.get("name") == "CI master"
        and run.get("event") == "push"
        and run.get("head_branch") == "master"
        and positive_id(run.get("id"))
        and positive_id(run.get("workflow_id"))
        and positive_id(run.get("run_number"))
        and isinstance(run.get("head_sha"), str)
        and COMMIT.fullmatch(run["head_sha"]) is not None
    )


def compiler_artifact(artifact, run):
    """Require both the exact name and the API's owning-run identity."""
    if not isinstance(artifact, dict) or not positive_id(artifact.get("id")):
        return None
    name = artifact.get("name")
    match = ARTIFACT_NAME.fullmatch(name) if isinstance(name, str) else None
    owner = artifact.get("workflow_run") or {}
    if not match or not isinstance(owner, dict):
        return None
    if (
        match[2] != run["head_sha"]
        or owner.get("id") != run["id"]
        or owner.get("head_sha") != run["head_sha"]
        or owner.get("head_branch") != "master"
        or owner.get("repository_id") != run["repository"]["id"]
        or owner.get("head_repository_id") != run["head_repository"]["id"]
    ):
        return None
    return match[1]


def require_replacement(api, current):
    artifacts = list_artifacts(api, f"{API_ROOT}/runs/{current['id']}/artifacts")
    platforms = []
    for artifact in artifacts:
        platform = compiler_artifact(artifact, current)
        if platform and artifact.get("expired") is False and positive_id(artifact.get("size_in_bytes")):
            platforms.append(platform)
    if len(platforms) != len(PLATFORMS) or set(platforms) != PLATFORMS:
        raise ValueError("Current run has no complete compiler set; keeping old artifacts")


def older_completed_run(run, current):
    return (
        trusted_run(run)
        and run["repository"]["id"] == current["repository"]["id"]
        and run["head_repository"]["id"] == current["head_repository"]["id"]
        and run["workflow_id"] == current["workflow_id"]
        and run["id"] != current["id"]
        # run_number stays unchanged when an old workflow is rerun. Its later
        # attempt must never prune artifacts belonging to a newer commit run.
        and run["run_number"] < current["run_number"]
        and run.get("status") == "completed"
    )


def cleanup(api, run_id, sha):
    current = api.request("GET", f"{API_ROOT}/runs/{run_id}")
    if not trusted_run(current) or current["id"] != run_id or current["head_sha"] != sha:
        raise ValueError("Cleanup requires the exact trusted CI master push run")
    require_replacement(api, current)

    # Enumerate retained artifacts, not the workflow's entire run history.
    # Only runs owning an exactly named compiler artifact need metadata lookups.
    artifacts = list_artifacts(api, f"{API_ROOT}/artifacts")
    runs = {}
    candidates = {}
    for artifact in artifacts:
        if not isinstance(artifact, dict) or not isinstance(artifact.get("name"), str):
            continue
        if not ARTIFACT_NAME.fullmatch(artifact["name"]):
            continue
        owner = artifact.get("workflow_run") or {}
        owner_id = owner.get("id") if isinstance(owner, dict) else None
        if not positive_id(owner_id) or owner_id == run_id:
            continue
        if owner_id not in runs:
            runs[owner_id] = api.request("GET", f"{API_ROOT}/runs/{owner_id}")
        run = runs[owner_id]
        if older_completed_run(run, current) and compiler_artifact(artifact, run):
            candidates.setdefault(owner_id, {})[artifact["id"]] = artifact

    # All pagination and initial metadata reads have succeeded before any delete.
    # Revalidate replacements in case a concurrent rerun replaced their uploads.
    require_replacement(api, current)
    deleted = 0
    for owner_id, group in candidates.items():
        run = api.request("GET", f"{API_ROOT}/runs/{owner_id}")
        if not older_completed_run(run, current):
            continue
        for artifact in group.values():
            if not compiler_artifact(artifact, run):
                continue
            try:
                api.request("DELETE", f"{API_ROOT}/artifacts/{artifact['id']}")
                deleted += 1
                print(f"Removed {artifact['name']} from completed run {owner_id}")
            except APIError as error:
                if error.status != 404:
                    raise
                print(f"Artifact {artifact['id']} was already removed")
    return deleted


def main():
    if (
        os.environ.get("GITHUB_REPOSITORY") != REPOSITORY
        or os.environ.get("GITHUB_WORKFLOW") != "CI master"
        or os.environ.get("GITHUB_EVENT_NAME") != "push"
        or os.environ.get("GITHUB_REF") != "refs/heads/master"
    ):
        raise ValueError("Compiler cleanup is restricted to vlang/v CI master pushes")
    run_id = int(os.environ.get("GITHUB_RUN_ID", "0"))
    sha = os.environ.get("GITHUB_SHA", "")
    if not positive_id(run_id) or not COMMIT.fullmatch(sha):
        raise ValueError("GITHUB_RUN_ID and GITHUB_SHA must identify the current run")
    deleted = cleanup(GitHubAPI(os.environ.get("GH_TOKEN")), run_id, sha)
    print(f"Removed {deleted} obsolete compiler artifacts")


if __name__ == "__main__":
    try:
        main()
    except (RuntimeError, ValueError) as error:
        print(f"Compiler artifact cleanup stopped: {error}", file=sys.stderr)
        sys.exit(1)
