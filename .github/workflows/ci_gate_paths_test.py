#!/usr/bin/env python3
"""Focused regression checks for CI push path selectors and gated callers."""
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
