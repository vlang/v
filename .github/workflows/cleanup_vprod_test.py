#!/usr/bin/env python3
"""Artifact cleanup regressions using an in-memory API; never contact GitHub."""
from contextlib import redirect_stdout
from copy import deepcopy
import io
import re
import unittest
from unittest.mock import patch
from urllib.parse import parse_qs, urlsplit

import cleanup_vprod as cleanup


def run(run_id, **changes):
    value = {
        "id": run_id,
        "run_number": run_id,
        "run_attempt": 1,
        "workflow_id": 50,
        "path": cleanup.WORKFLOW,
        "name": "CI master",
        "event": "push",
        "head_branch": "master",
        "head_sha": f"{run_id:040x}",
        "status": "completed",
        "repository": {"id": 169677297, "full_name": cleanup.REPOSITORY},
        "head_repository": {"id": 169677297, "full_name": cleanup.REPOSITORY},
    }
    value.update(changes)
    return value


def artifacts(owner):
    return [{
        "id": owner["id"] * 10 + index,
        "name": f"vprod-{platform}-{owner['head_sha']}",
        "expired": False,
        "size_in_bytes": 1234,
        "workflow_run": {
            "id": owner["id"],
            "head_sha": owner["head_sha"],
            "head_branch": owner["head_branch"],
            "repository_id": owner["repository"]["id"],
            "head_repository_id": owner["head_repository"]["id"],
        },
    } for index, platform in enumerate(sorted(cleanup.PLATFORMS))]


class FakeAPI:
    def __init__(self, runs, items):
        self.runs = {item["id"]: deepcopy(item) for item in runs}
        self.artifacts = deepcopy(items)
        self.calls = []
        self.deleted = []
        self.errors = {}
        self.run_reads = {}
        self.on_recheck = {}

    def request(self, method, path):
        self.calls.append((method, path))
        if (method, path) in self.errors:
            raise cleanup.APIError(method, path, self.errors[method, path])
        address = urlsplit(path)
        match = re.fullmatch(r"repos/vlang/v/actions/runs/(\d+)", address.path)
        if method == "GET" and match:
            run_id = int(match[1])
            self.run_reads[run_id] = self.run_reads.get(run_id, 0) + 1
            if self.run_reads[run_id] > 1 and run_id in self.on_recheck:
                self.on_recheck[run_id](self)
            return deepcopy(self.runs[run_id])
        if method == "GET" and address.path.endswith("/artifacts"):
            selected = self.artifacts
            match = re.search(r"/runs/(\d+)/artifacts$", address.path)
            if match:
                run_id = int(match[1])
                selected = [a for a in selected if isinstance(a, dict)
                            and isinstance(a.get("workflow_run"), dict)
                            and a["workflow_run"].get("id") == run_id]
            query = parse_qs(address.query)
            size, page = int(query["per_page"][0]), int(query["page"][0])
            return {"artifacts": deepcopy(selected[(page - 1) * size:page * size])}
        if method == "DELETE":
            artifact_id = int(address.path.rsplit("/", 1)[1])
            self.deleted.append(artifact_id)
            self.artifacts = [a for a in self.artifacts
                              if not isinstance(a, dict) or a.get("id") != artifact_id]
            return None
        raise AssertionError((method, path))


class CleanupTests(unittest.TestCase):
    def setUp(self):
        self.current = run(200, status="in_progress")
        self.old = run(100)
        self.api = FakeAPI([self.current, self.old], artifacts(self.current) + artifacts(self.old))

    def clean(self):
        with redirect_stdout(io.StringIO()):
            return cleanup.cleanup(self.api, self.current["id"], self.current["head_sha"])

    def test_paginates_before_deleting_and_keeps_unrelated_artifacts(self):
        self.api.artifacts += [{"id": 9999, "name": "test-results"}]
        with patch.object(cleanup, "PAGE_SIZE", 2):
            self.assertEqual(self.clean(), 6)
        first_delete = next(i for i, call in enumerate(self.api.calls) if call[0] == "DELETE")
        pages = [i for i, (_, path) in enumerate(self.api.calls)
                 if path.startswith(f"{cleanup.API_ROOT}/artifacts?")]
        self.assertEqual(len(pages), 7)
        self.assertLess(max(pages), first_delete)
        self.assertEqual({a["id"] for a in self.api.artifacts},
                         {a["id"] for a in artifacts(self.current)} | {9999})

    def test_preserves_newer_current_active_and_queued_runs(self):
        for other in [run(300), run(80, status="in_progress"), run(70, status="queued"),
                      run(60, status="waiting"), run(50, status="requested")]:
            self.api.runs[other["id"]] = other
            self.api.artifacts += artifacts(other)
        self.assertEqual(self.clean(), 6)
        self.assertEqual(set(self.api.deleted), {a["id"] for a in artifacts(self.old)})

    def test_stale_rerun_cannot_remove_newer_run(self):
        self.api.runs[200].update(run_attempt=4, updated_at="2099-01-01T00:00:00Z")
        newer = run(300, updated_at="2026-01-01T00:00:00Z")
        self.api.runs[300] = newer
        self.api.artifacts += artifacts(newer)
        self.assertEqual(self.clean(), 6)
        self.assertTrue({a["id"] for a in artifacts(newer)}.isdisjoint(self.api.deleted))

    def test_incomplete_expired_or_duplicate_replacement_keeps_everything(self):
        for mode in ["missing", "expired", "duplicate", "empty"]:
            with self.subTest(mode=mode):
                self.setUp()
                if mode == "missing":
                    self.api.artifacts.pop(0)
                elif mode == "expired":
                    self.api.artifacts[0]["expired"] = True
                elif mode == "empty":
                    self.api.artifacts[0]["size_in_bytes"] = 0
                else:
                    self.api.artifacts.append(deepcopy(self.api.artifacts[0]))
                with self.assertRaises(ValueError):
                    self.clean()
                self.assertEqual(self.api.deleted, [])

    def test_missing_termux_replacement_keeps_old_compilers(self):
        for platform in ("termux-arm64", "termux-x64"):
            with self.subTest(platform=platform):
                self.setUp()
                name = f"vprod-{platform}-{self.current['head_sha']}"
                self.api.artifacts = [a for a in self.api.artifacts if a["name"] != name]
                with self.assertRaises(ValueError):
                    self.clean()
                self.assertEqual(self.api.deleted, [])

    def test_legacy_four_platform_set_is_cleaned_after_six_replacements(self):
        old_artifacts = [a for a in artifacts(self.old) if not a["name"].startswith("vprod-termux-")]
        self.api.artifacts = artifacts(self.current) + old_artifacts
        self.assertEqual(self.clean(), 4)
        self.assertEqual(set(self.api.deleted), {a["id"] for a in old_artifacts})

    def test_preserves_other_repositories_workflows_events_and_branches(self):
        changes = [
            {"repository": {"id": 77, "full_name": "someone/v"}},
            {"head_repository": {"id": 77, "full_name": "someone/v"}},
            {"repository": "malformed"},
            {"workflow_id": 51}, {"path": ".github/workflows/other.yml"},
            {"name": "Other CI"}, {"event": "workflow_dispatch"},
            {"event": "pull_request"}, {"head_branch": "feature"},
        ]
        for change in changes:
            with self.subTest(change=change):
                self.setUp()
                self.api.runs[100].update(change)
                self.assertEqual(self.clean(), 0)
                self.assertEqual(self.api.deleted, [])

    def test_malformed_names_and_owner_mismatches_are_preserved(self):
        base = artifacts(self.old)[0]
        cases = [
            {"name": "vprod-linux-arm64-" + self.old["head_sha"]},
            {"name": "vprod-termux-arm-" + self.old["head_sha"]},
            {"name": "vprod-linux-x64-" + "a" * 39},
            {"name": base["name"] + "-logs"},
            {"name": "vprod-linux-x64-" + self.current["head_sha"]},
            {"workflow_run": None}, {"workflow_run": "malformed"},
            {"id": True}, {"name": None},
        ]
        for field, value in [("head_sha", "f" * 40), ("head_branch", "feature"),
                             ("repository_id", 77), ("head_repository_id", 77)]:
            owner = deepcopy(base["workflow_run"])
            owner[field] = value
            cases.append({"workflow_run": owner})
        for change in cases:
            with self.subTest(change=change):
                self.setUp()
                malformed = deepcopy(base)
                malformed.update(change)
                self.api.artifacts = artifacts(self.current) + [malformed, None, "invalid"]
                self.assertEqual(self.clean(), 0)
                self.assertEqual(self.api.deleted, [])

    def test_run_restarted_after_snapshot_is_preserved(self):
        self.api.on_recheck[100] = lambda api: api.runs[100].update(status="queued", run_attempt=2)
        self.assertEqual(self.clean(), 0)
        self.assertEqual(self.api.deleted, [])

    def test_listing_or_metadata_failure_stops_before_any_deletion(self):
        for path in [f"{cleanup.API_ROOT}/artifacts?per_page=2&page=2",
                     f"{cleanup.API_ROOT}/runs/100"]:
            with self.subTest(path=path):
                self.setUp()
                self.api.errors["GET", path] = 503
                with patch.object(cleanup, "PAGE_SIZE", 2), self.assertRaises(cleanup.APIError):
                    self.clean()
                self.assertEqual(self.api.deleted, [])

    def test_pagination_bound_stops_before_any_deletion(self):
        with patch.object(cleanup, "PAGE_SIZE", 2), patch.object(cleanup, "MAX_PAGES", 1):
            with self.assertRaises(ValueError):
                self.clean()
        self.assertEqual(self.api.deleted, [])

    def test_delete_404_is_idempotent_but_other_errors_stop_cleanup(self):
        first = artifacts(self.old)[0]["id"]
        endpoint = f"{cleanup.API_ROOT}/artifacts/{first}"
        self.api.errors["DELETE", endpoint] = 404
        self.assertEqual(self.clean(), 5)
        self.setUp()
        self.api.errors["DELETE", endpoint] = 403
        with self.assertRaises(cleanup.APIError):
            self.clean()
        self.assertEqual(self.api.deleted, [])

    def test_current_run_must_match_the_trusted_context(self):
        for change in [{"head_sha": "f" * 40}, {"event": "workflow_dispatch"},
                       {"path": ".github/workflows/other.yml"}, {"id": 201}]:
            with self.subTest(change=change):
                self.setUp()
                self.api.runs[200].update(change)
                with self.assertRaises(ValueError):
                    self.clean()
                self.assertEqual(self.api.deleted, [])

    def test_untrusted_environment_never_creates_api_client(self):
        with patch.dict(cleanup.os.environ, {}, clear=True), patch.object(cleanup, "GitHubAPI") as api:
            with self.assertRaises(ValueError):
                cleanup.main()
            api.assert_not_called()


if __name__ == "__main__":
    unittest.main()
