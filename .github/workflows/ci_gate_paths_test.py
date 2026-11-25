#!/usr/bin/env python3
"""Focused regression checks for CI push path selectors and gated callers."""
import json
from pathlib import Path
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
        explicit_gates = {"gen_vc_ci"}
        for name, entry in self.entries.items():
            if entry["shard"] == "portability":
                key = Path(name).stem
                explicit_gates.add(f"inline_{key}" if entry.get("inline") else key)
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
                condition = re.search(r"(?m)^    if: (>-\n(?:^      .+\n)+|.+)", body)
                if name in explicit_gates:
                    self.assertIsNotNone(condition)
                    self.assertIn("!cancelled()", condition[1])
                    for dependency in needs:
                        self.assertIn(f"needs.{dependency}.result == 'success'", condition[1])
                elif name not in {"cleanup-vprod", "record-tested-commit"}:
                    self.assertIsNotNone(condition)
                    self.assertNotRegex(condition[1], r"\b(always|failure|cancelled)\s*\(")

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
        for job in ("linux_ci", "compiler_ci", "docker_ci", "docs_ci"):
            with self.subTest(job=job):
                self.assertEqual(
                    self.dependencies(jobs[job]), {"preflight", "gen_vc_ci"}
                )

    def test_master_lanes_have_independent_bounded_queues(self):
        linux = gate.CALLER.read_text(encoding="utf-8")
        portability = gate.LANE_WORKFLOWS["portability"].read_text(encoding="utf-8")
        self.assertIn("|| 'ci-master-linux' }}", linux)
        self.assertIn("group: ci-master-portability\n", portability)
        for source in (linux, portability):
            self.assertIn("  push:\n    branches: [master]\n", source)
            self.assertIn("  cancel-in-progress: false\n", source)
        self.assertIn("uses: ./.github/workflows/ci_master.yml", portability)
        self.assertIn("portability: true", portability)

    def test_linux_lane_never_selects_a_suite_with_other_platform_runners(self):
        jobs = self.caller_jobs()
        for name, entry in self.entries.items():
            with self.subTest(suite=name):
                if entry.get("inline"):
                    source = jobs[f"inline_{Path(name).stem}"]
                else:
                    source = (gate.ROOT / name).read_text(encoding="utf-8")
                portable = name == "termux_ci.yml" or re.search(
                    r"\b(?:macos|windows)-(?:latest|\d+)|runs-on: \[.*macOS", source
                ) is not None
                self.assertEqual(gate.should_run_in_lane(None, entry, "master"), not portable)
                self.assertEqual(
                    gate.should_run_in_lane(None, entry, "portability"),
                    portable or name == "gen_vc_ci.yml",
                )

    def test_portability_vc_generation_validates_without_using_the_deploy_queue(self):
        self.assertIn("validate_only: ${{ inputs.portability || false }}", self.caller_jobs()["gen_vc_ci"])
        source = (gate.ROOT / "gen_vc_ci.yml").read_text(encoding="utf-8")
        self.assertIn("inputs.validate_only && format('gen-vc-validation-{0}', github.run_id)", source)
        self.assertIn("&& (inputs.validate_only ||", source)
        self.assertIn("&& !inputs.validate_only &&", source)

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

    def test_bootstrap_failures_cannot_advance_the_lane_baseline(self):
        job = self.caller_jobs()["record-tested-commit"]
        gates = {"bootstrap-macos", "bootstrap-windows", "bootstrap-termux",
                 "windows-bootstrap-contracts"}
        self.assertEqual(self.dependencies(job), {"preflight", "gen_vc_ci"} | gates)
        self.assertIn("!inputs.portability ||", job)
        for gate_job in gates | {"preflight", "gen_vc_ci"}:
            self.assertIn(f"needs.{gate_job}.result == 'success'", job)

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

    def test_windows_bootstrap_reuses_tools_before_full_formatting_checks(self):
        source = (gate.ROOT / "bootstrapping_ci.yml").read_text(encoding="utf-8")
        bootstrap = source.split("\n  bootstrap-termux:", 1)[0]
        steps = re.split(r"^      - ", bootstrap, flags=re.M)[1:]
        commands = [
            r".\.github\workflows\setup_windows_tool_cache.ps1",
            "makev.bat",
            "& ./.github/workflows/prepare_v.ps1",
            r".\.github\workflows\setup_windows_tool_cache.ps1 -Verify",
            "./v.exe -silent test-fmt",
        ]
        positions = []
        for command in commands:
            with self.subTest(command=command):
                matches = [step for step in steps if re.search(
                    rf"^        (?:run: |  ){re.escape(command)}$", step, re.M
                )]
                self.assertEqual(len(matches), 1)
                step = matches[0]
                self.assertIn("        if: runner.os == 'Windows'\n", step)
                self.assertNotIn("continue-on-error:", step)
                positions.append(bootstrap.index(command))
        self.assertEqual(positions, sorted(positions))
        self.assertLess(positions[-1], bootstrap.index("Copy-Item v.exe"))
        self.assertIn(
            "    timeout-minutes: ${{ matrix.platform == 'macos-x64' && 60 || 40 }}\n",
            bootstrap,
        )

    def test_windows_bootstrap_isolates_vet_and_fmt_processes(self):
        source = (gate.ROOT / "bootstrapping_ci.yml").read_text(encoding="utf-8")
        bootstrap = source.split("\n  bootstrap-termux:", 1)[0]
        steps = re.split(r"^      - ", bootstrap, flags=re.M)[1:]
        command = "./v.exe -silent test-cleancode"
        checks = [step for step in steps if command in step]
        self.assertEqual(len(checks), 2)
        for stage, step in zip(("vet", "fmt"), checks):
            with self.subTest(stage=stage):
                self.assertIn("        if: runner.os == 'Windows'\n", step)
                self.assertRegex(step, rf"(?m)^          VTEST_CLEANCODE_STAGE: '?{stage}'?$")
                self.assertEqual(step.count(command), 1)
                self.assertIn(
                    command + "\n          if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }",
                    step,
                )
                self.assertNotIn("continue-on-error:", step)
        self.assertIn(r".\.github\workflows\setup_windows_tool_cache.ps1 -Verify", checks[0])
        self.assertIn("./v.exe -silent test-fmt", checks[1])
        self.assertLess(bootstrap.index(checks[1]), bootstrap.index("Copy-Item v.exe"))

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
        files = [str(gate.LANE_WORKFLOWS["portability"])]
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

    class FakeAPI:
        def __init__(self, runs, marked):
            self.runs = runs
            self.marked = marked
            self.paths = []

        def get(self, path):
            self.paths.append(path)
            if path.startswith("actions/workflows/"):
                return {"workflow_runs": self.runs}
            run_id = int(path.split("/")[2])
            names = [gate.TESTED_MARK] if run_id in self.marked else []
            return {"artifacts": [{"name": name, "expired": False} for name in names]}

    def test_base_is_the_newest_finished_run_with_started_suites(self):
        runs = [
            {"id": 9, "head_sha": "after", "conclusion": "success"},
            {"id": 8, "head_sha": "c8", "conclusion": "cancelled"},
            {"id": 7, "head_sha": "c7", "conclusion": "success"},
            {"id": 6, "head_sha": "c6", "conclusion": "failure"},
            {"id": 5, "head_sha": "c5", "conclusion": "success"},
        ]
        api = self.FakeAPI(runs, marked={8, 6, 5})
        # Run 8 was cancelled and run 7 never started its suites, so their commits
        # are tested again from run 6 on.
        for lane, workflow in gate.LANE_WORKFLOWS.items():
            with self.subTest(lane=lane):
                api.paths.clear()
                self.assertEqual(gate.tested_base(api, "after", 10, lane, lambda c, a: True), "c6")
                self.assertIn(f"actions/workflows/{workflow.name}/runs?", api.paths[0])
                self.assertIn("status=completed", api.paths[0])
                self.assertNotIn("actions/runs/8/artifacts?name=ci-tested-commit", api.paths)

    def test_base_skips_the_current_run_and_unrelated_history(self):
        runs = [
            {"id": 10, "head_sha": "c10", "conclusion": "success"},
            {"id": 4, "head_sha": "rewritten", "conclusion": "success"},
            {"id": 3, "head_sha": "c3", "conclusion": "timed_out"},
        ]
        api = self.FakeAPI(runs, marked={10, 4, 3})
        ancestor = lambda commit, after: commit != "rewritten"
        self.assertIsNone(gate.tested_base(api, "after", 10, "master", ancestor))

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
