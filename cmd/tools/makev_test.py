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

    def test_msvc_bootstrap_selects_v1(self):
        # MSVC's cl.exe is invoked directly (there is no "!msvc_exe!" variable
        # the way the other three bootstraps have), so this needs its own
        # assertion rather than joining the compiler loop above.
        commands = [
            line for line in routine("build_bootstrap_with_msvc").splitlines()
            if line.startswith("cl ") and '"%V_C_FILE%"' in line
        ]
        self.assertEqual(len(commands), 1)
        self.assertIn("%VC_BOOTSTRAP_DEFINE%", commands[0])

    def test_clang_generation_matches_the_stage_compiler(self):
        commands = routine("build_stage_with_clang").splitlines()
        configure = next(line for line in commands if line.startswith("set stage_vflags="))
        compile_c = next(line for line in commands if line.startswith('"!clang_exe!" '))
        self.assertIn('-cc "!clang_exe!"', configure)
        self.assertIn('-cflags "--target=!clang_target!"', configure)
        self.assertIn("--target=!clang_target!", compile_c)
        self.assertNotIn("%VC_BOOTSTRAP_DEFINE%", compile_c)

    def test_directly_linked_stage_does_not_require_gc(self):
        generate = next(
            line for line in routine("generate_stage_c").splitlines()
            if line.startswith('"%V_BOOTSTRAP%" ')
        )
        self.assertIn("-gc none", generate)

    def test_bootstrap_only_emits_c_and_never_spawns_a_stage_compiler(self):
        commands = [
            line for line in SOURCE.splitlines()
            if line.startswith('"%V_BOOTSTRAP%" ')
        ]
        self.assertEqual(len(commands), 1)
        self.assertIn('-o "%V_STAGE_C%" cmd/v', commands[0])
        self.assertIn("!stage_vflags!", commands[0])

    def test_all_stage_compilers_link_the_generated_c_directly(self):
        for compiler in ("tcc", "clang", "gcc", "msvc"):
            with self.subTest(compiler=compiler):
                body = routine("build_stage_with_" + compiler)
                self.assertIn("call :generate_stage_c", body)
                self.assertIn(
                    "call :generate_stage_c\n"
                    "if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!", body
                )
                prefix = "cl " if compiler == "msvc" else '"!' + compiler + '_exe!" '
                link = next(line for line in body.splitlines() if line.startswith(prefix))
                self.assertIn('"%V_STAGE_C%"', link)
                self.assertIn('"%V_STAGE%"', link)
                self.assertNotIn("%VC_BOOTSTRAP_DEFINE%", link)
                self.assertIn(
                    link + '\nset stage_error=!ERRORLEVEL!\n'
                    'call :try_delete "%V_STAGE_C%"', body
                )
                self.assertIn("exit /b !stage_error!", body)

    def test_all_routes_use_the_fresh_stage_to_build_the_installed_compiler(self):
        for label, stage in (
            ("build_fresh_v_with_tcc", "tcc"),
            ("clang_strap", "clang"),
            ("gcc_strap", "gcc"),
            ("msvc_strap", "!stage_compiler!"),
        ):
            with self.subTest(label=label):
                body = routine(label)
                self.assertIn("call :build_stage_with_" + stage, body)
                self.assertIn('"%V_STAGE%" %V_BOOTSTRAP_VFLAGS%', body)
                self.assertNotIn('"%V_BOOTSTRAP%" %V_BOOTSTRAP_VFLAGS%', body)

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
    def run_stage(self, compiler, workdir, generate_status=0, link_status=0, final_status=0):
        labels = ["generate_stage_c", "build_stage_with_" + compiler, "try_delete"]
        if compiler == "tcc":
            labels.append("build_fresh_v_with_tcc")
        body = "\n".join(routine(label) for label in labels)
        body = body.replace('"%V_BOOTSTRAP%" ', "call :stub_generate ")
        prefix = "cl " if compiler == "msvc" else '"!' + compiler + '_exe!" '
        body = body.replace(prefix, "call :stub_link ")
        body = body.replace('"%V_STAGE%" ', "call :stub_final ")
        for wait in (100, 250):
            body = body.replace(
                f"ping 192.0.2.1 -n 1 -w {wait} >nul", "rem No delay in tests"
            )
        label = (
            "build_fresh_v_with_tcc" if compiler == "tcc"
            else "build_stage_with_" + compiler
        )
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            "set V_STAGE_C=v_stage.c",
            "set V_STAGE=v_stage.exe",
            "set V_EXE=v.exe",
            "set V_UPDATED=v_up.exe",
            "set V_BOOTSTRAP_VFLAGS=-no-parallel",
            'set "tcc_exe=C:\\fake tools\\tcc.exe"',
            'set "gcc_exe=C:\\fake tools\\gcc.exe"',
            'set "clang_exe=C:\\fake tools\\clang.exe"',
            "set clang_target=x86_64-w64-mingw32",
            "call :" + label,
            "exit /b !ERRORLEVEL!",
            body,
            ":stub_generate",
            "echo generate>>steps.txt",
            # A bootstrap with broken process spawning can still emit C, but
            # must not be asked to build an executable or invoke a C compiler.
            ":stub_generate_args",
            'if "%~1" == "" exit /b 99',
            'if "%~1" == "-o" goto :stub_generate_output',
            "shift",
            "goto :stub_generate_args",
            ":stub_generate_output",
            "shift",
            'if not "%~1" == "v_stage.c" exit /b 98',
            "echo generated C>v_stage.c",
            "exit /b " + str(generate_status),
            ":stub_link",
            "echo link>>steps.txt",
            "if not exist v_stage.c exit /b 97",
            "echo fresh stage>v_stage.exe",
            "exit /b " + str(link_status),
            ":stub_final",
            "echo final>>steps.txt",
            "if not exist v_stage.exe exit /b 96",
            "echo fresh compiler>v_up.exe",
            "exit /b " + str(final_status),
        ])
        harness = workdir / "stage harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        return subprocess.run(
            [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
            cwd=workdir, capture_output=True, text=True, errors="replace",
            timeout=15, check=False,
        )

    def test_stage_builds_do_not_ask_the_bootstrap_to_spawn_a_compiler(self):
        for compiler in ("tcc", "clang", "gcc", "msvc"):
            with self.subTest(compiler=compiler), tempfile.TemporaryDirectory(
                prefix="makev tests "
            ) as directory:
                workdir = Path(directory)
                result = self.run_stage(compiler, workdir)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                expected = (
                    ["generate", "link", "final"] if compiler == "tcc"
                    else ["generate", "link"]
                )
                self.assertEqual((workdir / "steps.txt").read_text().splitlines(), expected)
                self.assertFalse((workdir / "v_stage.c").exists())

    def test_stage_generation_and_link_failures_preserve_their_exit_code(self):
        for compiler in ("tcc", "clang", "gcc", "msvc"):
            for generate_status, link_status, expected in (
                (37, 0, ["generate"]), (0, 41, ["generate", "link"])
            ):
                with self.subTest(
                    compiler=compiler, generate_status=generate_status,
                    link_status=link_status
                ), tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
                    workdir = Path(directory)
                    result = self.run_stage(compiler, workdir, generate_status, link_status)
                    self.assertEqual(
                        result.returncode, generate_status or link_status,
                        result.stdout + result.stderr
                    )
                    self.assertEqual((workdir / "steps.txt").read_text().splitlines(), expected)
                    self.assertFalse((workdir / "v_stage.c").exists())

    def test_final_tcc_build_failure_preserves_its_exit_code_after_cleanup(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            result = self.run_stage("tcc", workdir, final_status=43)
            self.assertEqual(result.returncode, 43, result.stdout + result.stderr)
            self.assertFalse((workdir / "v_stage.exe").exists())

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

    def run_try_delete(self, workdir, target):
        body = routine("try_delete")
        for wait in ("ping 192.0.2.1 -n 1 -w 100 >nul", "ping 192.0.2.1 -n 1 -w 250 >nul"):
            body = body.replace(wait, "rem No delay in tests")
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            'call :try_delete "' + str(target) + '"',
            "exit /b !ERRORLEVEL!",
            body,
        ])
        harness = workdir / "try_delete harness.bat"
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

    def test_try_delete_removes_an_existing_unlocked_file(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            target = workdir / "leftover.exe"
            target.write_text("stale binary", encoding="utf-8")
            result = self.run_try_delete(workdir, target)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertFalse(target.exists())

    def test_try_delete_succeeds_when_the_file_never_existed(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            target = workdir / "never_written.exe"
            result = self.run_try_delete(workdir, target)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_try_delete_succeeds_even_when_the_file_stays_locked(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            target = workdir / "locked.exe"
            target.write_text("stale binary", encoding="utf-8")
            with open(target, "rb"):
                result = self.run_try_delete(workdir, target)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertTrue(target.exists())


if __name__ == "__main__":
    unittest.main(verbosity=2)
