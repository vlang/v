"""Check Windows bootstrap contracts without requiring a working V compiler.

Run with: python cmd/tools/makev_test.py
On Windows, also exercise the real batch subroutines, stubbing only calls to V
and the installation delay. File replacement uses real temporary files.
"""

import os
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
SOURCE = (ROOT / "makev.bat").read_text(encoding="utf-8")


def routine(name: str) -> str:
    match = re.search(r"(?m)^:" + re.escape(name) + r"\n", SOURCE)
    if match is None:
        raise AssertionError("Missing makev.bat subroutine: " + name)
    end = re.search(r"(?m)^:[^\n]+$", SOURCE[match.end():])
    stop = match.end() + end.start() if end else len(SOURCE)
    return SOURCE[match.start():stop]


class BootstrapConfigurationTests(unittest.TestCase):
    def test_all_portable_bootstraps_select_v1(self):
        self.assertIn(
            "set VC_BOOTSTRAP_DEFINE=-DCUSTOM_DEFINE_v1_fallback\n", SOURCE
        )
        for compiler in ("tcc", "clang", "gcc"):
            with self.subTest(compiler=compiler):
                commands = [
                    line for line in routine("build_bootstrap_with_" + compiler).splitlines()
                    if line.startswith('"!' + compiler + '_exe!" ')
                    and '"%V_C_FILE%"' in line
                ]
                self.assertEqual(len(commands), 1)
                self.assertIn("%VC_BOOTSTRAP_DEFINE%", commands[0])

    def test_clang_generation_matches_the_stage_compiler(self):
        commands = routine("build_stage_with_clang").splitlines()
        generate = next(line for line in commands if line.startswith('"%V_BOOTSTRAP%" '))
        compile_c = next(line for line in commands if line.startswith('"!clang_exe!" '))
        self.assertIn('-cc "!clang_exe!"', generate)
        self.assertIn('-cflags "--target=!clang_target!"', generate)
        self.assertIn("--target=!clang_target!", compile_c)
        self.assertNotIn("%VC_BOOTSTRAP_DEFINE%", compile_c)

    def test_directly_linked_stage_does_not_require_gc(self):
        generate = next(
            line for line in routine("build_stage_with_clang").splitlines()
            if line.startswith('"%V_BOOTSTRAP%" ')
        )
        self.assertIn("-gc none", generate)

    def test_install_does_not_build_a_fake_compatibility_compiler(self):
        install = routine("move_updated_to_v")
        self.assertNotIn("-d v1_fallback", install)
        self.assertNotIn("V1_FALLBACK", SOURCE)
        self.assertNotIn("V_FALLBACK_CC_ARGS", SOURCE)

    def test_failed_checks_and_postbuild_tools_are_not_masked(self):
        self.assertIn('"%V_EXE%" test-all\nexit /b !ERRORLEVEL!', routine("check"))
        for label, command in (
            ("success", '"%V_EXE%" run cmd/tools/detect_tcc.v'),
            ("version", '"%V_EXE%" version'),
            ("version", '"%V_EXE%" run .github/problem-matchers/register_all.vsh'),
        ):
            with self.subTest(command=command):
                self.assertIn(
                    command + "\nif !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!",
                    routine(label),
                )


@unittest.skipUnless(os.name == "nt", "requires the Windows cmd.exe interpreter")
class BatchExecutionTests(unittest.TestCase):
    def run_batch(self, label, workdir, statuses=None, old_path=None):
        statuses = statuses or {}
        commands = {
            "check": '"%V_EXE%" test-all',
            "detect": '"%V_EXE%" run cmd/tools/detect_tcc.v',
            "version": '"%V_EXE%" version',
            "register": '"%V_EXE%" run .github/problem-matchers/register_all.vsh',
        }
        body = "\n".join(
            routine(name) for name in ("check", "success", "version", "move_updated_to_v")
        )
        for name, command in commands.items():
            self.assertEqual(body.count(command + "\n"), 1)
            body = body.replace(
                command + "\n",
                '"%ComSpec%" /d /c exit ' + str(statuses.get(name, 0)) + "\n",
            )
        body = body.replace(
            "ping 192.0.2.1 -n 1 -w 100 >nul", "rem No installation delay in tests"
        )
        old_path = old_path or workdir / "v_old.exe"
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            'set "V_EXE=' + str(workdir / "v.exe") + '"',
            'set "V_UPDATED=' + str(workdir / "v_up.exe") + '"',
            'set "V_OLD=' + str(old_path) + '"',
            "call :" + label,
            "exit /b !ERRORLEVEL!",
            body,
        ])
        harness = workdir / "makev harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        return subprocess.run(
            [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
            cwd=workdir,
            capture_output=True,
            text=True,
            errors="replace",
            timeout=15,
            check=False,
        )

    def test_check_preserves_test_runner_exit_code(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            for code in (0, 37):
                with self.subTest(code=code):
                    result = self.run_batch("check", Path(directory), {"check": code})
                    self.assertEqual(result.returncode, code, result.stdout + result.stderr)

    def test_postbuild_failures_preserve_exit_code(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            for command in ("detect", "version", "register"):
                with self.subTest(command=command):
                    result = self.run_batch("success", Path(directory), {command: 37})
                    self.assertEqual(result.returncode, 37, result.stdout + result.stderr)

    def test_postbuild_success(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result = self.run_batch("success", Path(directory))
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_missing_update_preserves_existing_compiler(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
            result = self.run_batch("move_updated_to_v", workdir)
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual((workdir / "v.exe").read_text(), "working compiler")
            self.assertFalse((workdir / "v_old.exe").exists())

    def test_failed_backup_does_not_replace_existing_compiler(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
            (workdir / "v_up.exe").write_text("new compiler", encoding="utf-8")
            result = self.run_batch(
                "move_updated_to_v", workdir, old_path=workdir / "missing" / "v_old.exe"
            )
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual((workdir / "v.exe").read_text(), "working compiler")
            self.assertEqual((workdir / "v_up.exe").read_text(), "new compiler")

    def test_install_replaces_compiler_and_previous_backup(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
            (workdir / "v_up.exe").write_text("new compiler", encoding="utf-8")
            (workdir / "v_old.exe").write_text("previous backup", encoding="utf-8")
            result = self.run_batch("move_updated_to_v", workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual((workdir / "v.exe").read_text(), "new compiler")
            self.assertEqual((workdir / "v_old.exe").read_text(), "working compiler")
            self.assertFalse((workdir / "v_up.exe").exists())


if __name__ == "__main__":
    unittest.main(verbosity=2)
