#!/usr/bin/env python3
"""Focused regression checks for CI push path selectors and gated callers."""
import json
import re
import unittest
import ci_gate_paths as gate


class GatePathsTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.entries = {e["workflow"]: e for e in gate.load_entries()}

    def test_manifest_and_callers_are_complete(self):
        gate.validate(list(self.entries.values()))
        self.assertEqual(len(self.entries), 54)
        self.assertEqual(sum(bool(e.get("inline")) for e in self.entries.values()), 8)

    def test_vc_generation_runs_on_every_master_push(self):
        for paths in ([], ["README.md"], [".github/workflows/linux_ci.yml"]):
            with self.subTest(paths=paths):
                self.assertTrue(gate.should_run(paths, self.entries["gen_vc_ci.yml"]))

    @staticmethod
    def caller_jobs():
        caller = gate.CALLER.read_text(encoding="utf-8").split("\njobs:\n", 1)[1]
        return dict(re.findall(r"^  ([\w-]+):\n(.*?)(?=^  \S|\Z)", caller, re.M | re.S))

    @staticmethod
    def dependencies(job):
        match = re.search(r"^    needs:\s*(\[[^\]]*\]|[^\n]+)$", job, re.M)
        if match is None:
            return None
        return {name.strip() for name in match[1].strip("[]").split(",") if name.strip()}

    def test_dependencies_accept_prettier_wrapping(self):
        expected = {"preflight", "gen_vc_ci"}
        for value in (
            " [preflight, gen_vc_ci]",
            "\n      [preflight, gen_vc_ci]",
            "\n      [\n        preflight,\n        gen_vc_ci,\n      ]",
        ):
            with self.subTest(value=value):
                self.assertEqual(self.dependencies(f"    needs:{value}\n"), expected)

    def test_vc_generation_gates_every_downstream_job(self):
        jobs = self.caller_jobs()
        preflight = {
            "bootstrap-linux", "bootstrap-macos", "bootstrap-windows",
            "bootstrap-termux", "windows-bootstrap-contracts", "preflight",
        }
        self.assertIn("gen_vc_ci", jobs)
        for name, body in jobs.items():
            if name in preflight:
                continue
            with self.subTest(job=name):
                needs = self.dependencies(body)
                self.assertIsNotNone(needs)
                if name == "gen_vc_ci":
                    self.assertEqual(needs, {"preflight"})
                else:
                    self.assertIn("gen_vc_ci", needs)
                    if name != "cleanup-vprod":
                        self.assertIn("preflight", needs)
                self.assertNotRegex(body, r"(?m)^    if:.*\b(always|failure|cancelled)\s*\(")

    def test_linux_preflight_does_not_wait_for_other_platforms(self):
        jobs = self.caller_jobs()
        for platform in ("linux", "macos", "windows", "termux"):
            with self.subTest(platform=platform):
                body = jobs[f"bootstrap-{platform}"]
                self.assertIn("uses: ./.github/workflows/bootstrapping_ci.yml", body)
                self.assertIn(f"platform: {platform}", body)
                self.assertIsNone(self.dependencies(body))
        self.assertEqual(self.dependencies(jobs["preflight"]), {"bootstrap-linux"})
        self.assertEqual(self.dependencies(jobs["gen_vc_ci"]), {"preflight"})
        for job in ("linux_ci", "compiler_ci", "tools_ci", "cross_ci", "release_ci"):
            with self.subTest(job=job):
                self.assertEqual(
                    self.dependencies(jobs[job]), {"preflight", "gen_vc_ci"}
                )

    def test_other_platforms_retain_bootstrap_gates(self):
        jobs = self.caller_jobs()
        for name in ("macos_ci", "macos_gitly_ci"):
            self.assertIn("bootstrap-macos", self.dependencies(jobs[name]))
        for name in ("windows_ci_gcc", "windows_ci_msvc", "windows_ci_tcc"):
            self.assertTrue(
                {"bootstrap-windows", "windows-bootstrap-contracts"}
                <= self.dependencies(jobs[name])
            )
        self.assertIn("bootstrap-termux", self.dependencies(jobs["termux_ci"]))
        self.assertEqual(
            self.dependencies(jobs["cleanup-vprod"]),
            {"gen_vc_ci", "bootstrap-macos", "bootstrap-windows",
             "bootstrap-termux", "windows-bootstrap-contracts"},
        )

    def test_bootstrap_platform_matrix_matches_all_producers(self):
        source = (gate.ROOT / "bootstrapping_ci.yml").read_text(encoding="utf-8")
        platforms = {}
        for group in ("linux", "macos", "windows"):
            match = re.search(
                rf"inputs\.platform == '{group}' && '(\[[^'\n]+\])'", source
            )
            self.assertIsNotNone(match, group)
            platforms[group] = json.loads(match[1])
        all_match = re.search(r"(?m)^\s*'(\[\{\"os\":[^'\n]+\])'$", source)
        self.assertIsNotNone(all_match)
        combined = json.loads(all_match[1])
        self.assertCountEqual(
            combined, [runner for group in platforms.values() for runner in group]
        )
        self.assertEqual([r["platform"] for r in platforms["linux"]], ["linux-x64"])
        self.assertEqual(
            {r["platform"] for r in platforms["macos"]}, {"macos-arm64", "macos-x64"}
        )
        self.assertEqual([r["platform"] for r in platforms["windows"]], ["windows-x64"])
        self.assertIn("inputs.platform != 'termux'", source)
        self.assertIn("inputs.platform == 'all' || inputs.platform == 'termux'", source)
        self.assertIn("inputs.platform || 'all'", source)

    def test_fastc_only_push_skips_platform_workflows(self):
        paths = ["vlib/v/gen/fastc/codegen.v"]
        self.assertTrue(gate.should_run(paths, self.entries["fastc_ci.yml"]))
        self.assertTrue(gate.should_run(paths, self.entries["compiler_ci.yml"]))
        self.assertFalse(gate.should_run(paths, self.entries["linux_ci.yml"]))
        self.assertFalse(gate.should_run(paths, self.entries["windows_ci_gcc.yml"]))

    def test_ignore_then_reinclude_builders(self):
        self.assertTrue(gate.should_run(
            ["cmd/tools/builders/build.v"], self.entries["linux_ci.yml"]
        ))
        self.assertFalse(gate.should_run(
            ["cmd/tools/mytool.v"], self.entries["linux_ci.yml"]
        ))

    def test_docker_musl_harness_change_selects_docker_ci(self):
        self.assertTrue(gate.should_run(
            ["cmd/tools/test_if_v_test_system_works.v"], self.entries["docker_ci.yml"]
        ))
        self.assertFalse(gate.should_run(
            ["cmd/tools/unrelated.v"], self.entries["docker_ci.yml"]
        ))

    def test_workflow_only_changes_retain_lint(self):
        files = [".github/workflows/ci_master_core.yml"]
        self.assertTrue(gate.should_run(files, self.entries["workflow_lint.yml"]))
        self.assertFalse(gate.should_run(files, self.entries["other_ci.yml"]))

    def test_caller_changes_run_formerly_self_selecting_inlined_workflows(self):
        files = [str(gate.CALLER)]
        for workflow in (
            "benchmark_footprint_json_decode.yml",
            "benchmark_footprint_json_encode.yml",
            "h2spec.yml",
            "toml_ci.yml",
            "vinix_ci.yml",
            "wasm_backend_ci.yml",
            "workflow_lint.yml",
        ):
            with self.subTest(workflow=workflow):
                self.assertTrue(gate.should_run(files, self.entries[workflow]))
        self.assertFalse(gate.should_run(files, self.entries["module_docs_lint.yml"]))

    def test_unfiltered_workflow_runs_on_empty_diff(self):
        self.assertTrue(gate.should_run([], self.entries["tccbin_automation_contract.yml"]))
        self.assertFalse(gate.should_run([], self.entries["wasm_backend_ci.yml"]))
        self.assertFalse(gate.should_run([], self.entries["linux_ci.yml"]))

    def test_failure_to_compute_diff_runs_all(self):
        self.assertTrue(all(gate.should_run(None, e) for e in self.entries.values()))

    def test_shared_compiler_changes_run_all_consumers(self):
        for path in sorted(gate.SHARED_COMPILER_FILES | {".github/actions/setup-v/action.yml"}):
            with self.subTest(path=path):
                self.assertTrue(all(gate.should_run([path], e) for e in self.entries.values()))

    def test_ordered_negative_patterns(self):
        self.assertFalse(gate.path_is_selected(
            "vlib/json2/strict/tokenizer.v", "paths",
            ["vlib/json2/**", "!vlib/json2/strict/**"],
        ))
        self.assertTrue(gate.path_is_selected(
            "vlib/json2/strict/tokenizer.v", "paths",
            ["vlib/json2/**", "!vlib/json2/strict/**", "vlib/json2/strict/**"],
        ))


if __name__ == "__main__":
    unittest.main()
