#!/usr/bin/env python3
"""Exercise VTL command sharding with a local compiler stub; no network is used."""

from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile
import unittest


ADAPTER = Path(__file__).resolve().parents[2] / "ci" / "vtl_v_shard.py"
WORKFLOW = ADAPTER.parents[1] / ".github" / "workflows" / "vsl_and_vtl_compile_ci.yml"
PROBE = '''#!/usr/bin/env python3
import json, os, subprocess, sys
with open(os.environ["VTL_TEST_LOG"], "a") as log:
    log.write(json.dumps({"args": sys.argv[1:], "path": os.environ["PATH"],
        "adapter_env": [k for k in os.environ if k.startswith("VTL_CI_")],
        "vflags": os.environ.get("VFLAGS"), "vjobs": os.environ.get("VJOBS")}) + "\\n")
if os.environ.get("VTL_TEST_NESTED") and sys.argv[1:] != ["nested"]:
    subprocess.run(["v", "nested"], check=True)
sys.exit(int(os.environ.get("VTL_TEST_STATUS", "0")))
'''


class VtlShardingTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="v VTL sharding ")
        self.addCleanup(self.temp.cleanup)
        base = Path(self.temp.name)
        self.root = base / "module"
        self.root.mkdir()
        self.real_bin = base / "real bin"
        self.real_bin.mkdir()
        self.compiler = self.real_bin / "v"
        self.compiler.write_text(PROBE)
        self.compiler.chmod(0o755)
        self.wrapper_bin = base / "shard bin"
        self.wrapper_bin.mkdir()
        wrapper = self.wrapper_bin / "v"
        wrapper.write_bytes(ADAPTER.read_bytes())
        wrapper.chmod(0o755)
        self.log = base / "calls.jsonl"
        self.original_path = str(self.real_bin) + os.pathsep + os.environ["PATH"]
        self.environment = dict(os.environ, VTL_TEST_LOG=str(self.log),
            VTL_CI_ROOT=str(self.root), VTL_CI_VEXE=str(self.compiler),
            VTL_CI_PATH=self.original_path, VTL_CI_SHARD_COUNT="3",
            VFLAGS="-no-parallel", VJOBS="1",
            PATH=str(self.wrapper_bin) + os.pathsep + self.original_path)

    def source(self, relative):
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("fn main() {}\n")
        return str(path)

    def call(self, args, shard, **environment):
        env = dict(self.environment, VTL_CI_SHARD_INDEX=str(shard), **environment)
        return subprocess.run(["v", *args], env=env, text=True, capture_output=True, timeout=5)

    def records(self):
        return [json.loads(line) for line in self.log.read_text().splitlines()] if self.log.exists() else []

    def workflow_script(self, job, step):
        # Like tools_ci_test.v, read only the known job/literal-block shape.
        # A changed or ambiguous structure must fail instead of testing a copy.
        workflow = WORKFLOW.read_text()
        marker = f"  {job}:\n"
        self.assertEqual(workflow.count(marker), 1)
        body = re.split(r"^  \S", workflow.split(marker)[1], maxsplit=1, flags=re.MULTILINE)[0]
        marker = f"      - name: {step}\n"
        self.assertEqual(body.count(marker), 1)
        lines = body.split(marker)[1].splitlines()
        while lines and not lines[0].startswith("        run:"):
            lines.pop(0)
        self.assertTrue(lines and lines.pop(0) == "        run: |")
        script = []
        for line in lines:
            if not line.startswith("          "):
                break
            script.append(line[10:])
        self.assertTrue(script)
        return "\n".join(script) + "\n"

    def test_tests_packages_examples_and_smokes_execute_exactly_once(self):
        test = self.source("tests/core/array_test.v")
        self.source("stats/mean_test.v")
        example = self.source("examples/with spaces/main.v")
        requests = [["test", test], ["-d", "cblas", "test", str(self.root / "stats")],
            ["-no-parallel", "-o", example + ".bin", example],
            ["-d", "cblas", "-no-parallel", "run", example]]
        for shard in range(3):
            for request in requests:
                result = self.call(request, shard)
                self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(Counter(tuple(r["args"]) for r in self.records()),
                         Counter(tuple(r) for r in requests))
        for record in self.records():
            self.assertEqual(record["path"], self.original_path)
            self.assertEqual(record["adapter_env"], [])
            self.assertEqual(record["vflags"], "-no-parallel")
            self.assertEqual(record["vjobs"], "1")

    def test_nested_v_calls_are_not_sharded(self):
        test = self.source("nested_test.v")
        for shard in range(3):
            result = self.call(["test", test], shard, VTL_TEST_NESTED="1")
            self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual([r["args"] for r in self.records()], [["test", test], ["nested"]])

    def test_compiler_failure_is_preserved(self):
        test = self.source("failure_test.v")
        results = [self.call(["test", test], shard, VTL_TEST_STATUS="43") for shard in range(3)]
        self.assertEqual(sorted(result.returncode for result in results), [0, 0, 43])
        self.assertEqual(len(self.records()), 1)

    def test_assignment_is_relative_to_module_and_independent_of_backend(self):
        relative = "tests/core/stable_test.v"
        expected = int.from_bytes(hashlib.sha256(relative.encode()).digest()[:8], "big") % 3
        for root_name in ["module", "another checkout"]:
            self.root = Path(self.temp.name) / root_name
            self.root.mkdir(exist_ok=True)
            self.environment["VTL_CI_ROOT"] = str(self.root)
            test = self.source(relative)
            for flags in [[], ["-d", "cblas"]]:
                for shard in range(3):
                    result = self.call([*flags, "test", test], shard)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn(": run test " if shard == expected else ": skip test ", result.stdout)
        self.assertEqual(len(self.records()), 4)

    def test_unsupported_commands_flags_and_targets_fail_on_every_shard(self):
        test = self.source("valid_test.v")
        outside = Path(self.temp.name) / "outside_test.v"
        outside.write_text("fn test_outside() {}\n")
        requests = [["version"], ["fmt", test], ["-unknown", "test", test],
            ["-d", "test", test], ["test", str(outside)],
            ["run", str(self.root)], ["-o", str(outside), test]]
        for request in requests:
            for shard in range(3):
                result = self.call(request, shard)
                self.assertEqual(result.returncode, 2, (request, result.stdout, result.stderr))
                self.assertIn("VTL CI sharding:", result.stderr)
        self.assertEqual(self.records(), [])

    def test_invalid_shards_fail_instead_of_succeeding_empty(self):
        test = self.source("valid_test.v")
        for count, index in [("0", "0"), ("3", "3"), ("3", "-1"), ("3", "01"), ("x", "0")]:
            result = self.call(["test", test], index, VTL_CI_SHARD_COUNT=count)
            self.assertEqual(result.returncode, 2, result.stderr)
        self.assertEqual(self.records(), [])

    def test_workflow_resolves_both_revisions_and_rejects_invalid_output(self):
        script = self.workflow_script("module-revisions", "Resolve upstream revisions once for every backend and shard")
        git = self.real_bin / "git"
        git.write_text('''#!/bin/sh
case "$3" in
  https://github.com/vlang/vsl.git) printf '%s\\tHEAD\\n' "$VSL_TEST_SHA" ;;
  https://github.com/vlang/vtl.git) printf '%s\\tHEAD\\n' "$VTL_TEST_SHA" ;;
  *) exit 97 ;;
esac
''')
        git.chmod(0o755)
        output = Path(self.temp.name) / "github-output"
        for vtl_sha in ["b" * 40, "invalid"]:
            output.write_text("")
            env = dict(os.environ, PATH=self.original_path, GITHUB_OUTPUT=str(output),
                       VSL_TEST_SHA="a" * 40, VTL_TEST_SHA=vtl_sha)
            result = subprocess.run(["bash", "-e", "-o", "pipefail", "-c", script], env=env,
                                    capture_output=True, text=True, timeout=5)
            if vtl_sha == "invalid":
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("vtl=", output.read_text())
            else:
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(output.read_text().splitlines(), ["vsl=" + "a" * 40, "vtl=" + vtl_sha])

    def test_workflow_installs_selected_revision_after_upstream_moves(self):
        origin = Path(self.temp.name) / "upstream"
        origin.mkdir()

        def git(*args):
            return subprocess.check_output(["git", "-c", "user.name=CI fixture", "-c",
                "user.email=ci@example.invalid", "-c", "commit.gpgsign=false", *args],
                cwd=origin, text=True, stderr=subprocess.DEVNULL).strip()

        git("init")
        (origin / "v.mod").write_text("Module { name: 'fixture' dependencies: [] }\n")
        git("add", ".")
        git("commit", "-m", "Selected revision")
        selected = git("rev-parse", "HEAD")
        (origin / "later_test.v").write_text("fn test_later() {}\n")
        git("add", ".")
        git("commit", "-m", "Upstream moved while shards queued")
        self.compiler.write_text('''#!/bin/sh
if [ "$1" = "retry" ]; then
  shift; test "$1" = "--" || exit 98; shift
  if [ "${VTL_TEST_FAIL_SUBMODULE:-0}" = "1" ] && [ "${4:-}" = "submodule" ]; then exit 44; fi
  exec "$@"
fi
if [ "$1" = "install" ]; then git rev-parse HEAD > "$VTL_TEST_PIN_LOG"; exit 0; fi
exit 97
''')
        for module in ["vsl", "vtl"]:
            original = self.workflow_script(f"{module}-compiles", f"Install {module}")
            assignment = f'{module}_dir="$HOME/.vmodules/{module}"'
            self.assertIn(assignment, original)
            self.assertIn(f"https://github.com/vlang/{module}.git", original)
            for revision, fail_submodule in [(selected, False), ("0" * 40, False), (selected, True)]:
                case = f"{module}-{revision}-{fail_submodule}"
                destination = Path(self.temp.name) / case
                marker = Path(self.temp.name) / f"{case}.installed"
                script = original.replace(assignment, f"{module}_dir={shlex.quote(str(destination))}")
                script = script.replace(f"https://github.com/vlang/{module}.git", shlex.quote(str(origin)))
                env = dict(os.environ, PATH=self.original_path, VTL_TEST_PIN_LOG=str(marker),
                           VTL_TEST_FAIL_SUBMODULE=str(int(fail_submodule)),
                           **{f"{module.upper()}_CI_REVISION": revision})
                result = subprocess.run(["bash", "-e", "-o", "pipefail", "-c", script], env=env,
                                        capture_output=True, text=True, timeout=10)
                if revision == selected and not fail_submodule:
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(marker.read_text().strip(), selected)
                    self.assertFalse((destination / "later_test.v").exists())
                    # VPM rejects clean detached checkouts without a remote ref
                    # when they appear to contain unpushed local commits.
                    local_commits = subprocess.check_output(["git", "-C", str(destination),
                        "rev-list", "HEAD", "--branches", "--not", "--remotes", "--tags"], text=True)
                    self.assertEqual(local_commits, "")
                else:
                    self.assertNotEqual(result.returncode, 0)
                    if fail_submodule:
                        self.assertEqual(result.returncode, 44)
                    self.assertFalse(marker.exists(), "installed dependencies after a failed checkout")


if __name__ == "__main__":
    unittest.main()
