"""Check Windows bootstrap contracts without requiring a working V compiler.

Run with: python cmd/tools/makev_test.py
On Windows, also exercise the real batch subroutines with compiler calls and
installation delays stubbed. File replacement uses real temporary files.
When bundled TCC is available, also link and run a native bootstrap helper probe.
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
                    if line.lstrip().startswith('"!' + compiler + '_exe!" ')
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
            if line.lstrip().startswith("cl ") and '"%V_C_FILE%"' in line
        ]
        self.assertEqual(len(commands), 1)
        self.assertIn("%VC_BOOTSTRAP_DEFINE%", commands[0])

    def test_all_bootstraps_link_the_external_windows_helpers(self):
        native = next(
            line for line in SOURCE.splitlines()
            if line.startswith("set V_BOOTSTRAP_NATIVE=")
        )
        for path in (
            r"vlib\time\ticks_windows.c",
            r"vlib\v\winthread_helper_windows.c",
            r"cmd\v\toolcache_windows_helpers.c",
        ):
            self.assertIn('"%~dp0' + path + '"', native)
        for compiler in ("tcc", "clang", "gcc", "msvc"):
            with self.subTest(compiler=compiler):
                commands = [
                    line for line in routine("build_bootstrap_with_" + compiler).splitlines()
                    if '"%V_C_FILE%"' in line and not line.startswith("echo ")
                ]
                self.assertEqual(len(commands), 2 if compiler == "clang" else 1)
                for command in commands:
                    self.assertIn("%V_BOOTSTRAP_NATIVE%", command)

    def test_tcc_links_select_the_installed_bundle_openlibm(self):
        # Every Windows TCC bundle ships lib/openlibm.o for its own compiler. An
        # existing checkout keeps its branch, so never infer it from tcc_branch.
        select = [
            line for line in routine("select_tcc_openlibm").splitlines()
            if line and not line.startswith("REM ")
        ]
        self.assertEqual(select, [
            ":select_tcc_openlibm",
            'set "TCC_OPENLIBM="',
            'if exist "%tcc_dir%\\lib\\openlibm.o" set TCC_OPENLIBM="%tcc_dir%\\lib\\openlibm.o"',
            "exit /b 0",
        ])
        for compiler in ("tcc", "clang", "gcc", "msvc"):
            for stage in ("bootstrap", "stage"):
                with self.subTest(compiler=compiler, stage=stage):
                    body = routine("build_" + stage + "_with_" + compiler)
                    prefix = "cl " if compiler == "msvc" else '"!' + compiler + '_exe!" '
                    links = [line for line in body.splitlines() if line.lstrip().startswith(prefix)
                             and ('"%V_C_FILE%"' in line or '"%V_STAGE_C%"' in line)]
                    self.assertEqual(len(links), 1)
                    if compiler != "tcc":
                        self.assertNotIn("OPENLIBM", body.upper())
                        continue
                    # Select in each routine, right before its link: the stage
                    # link also runs after a Clang/GCC bootstrap.
                    lines = body.splitlines()
                    select_at = lines.index("call :select_tcc_openlibm")
                    link_at = lines.index(links[0])
                    self.assertLess(select_at, link_at)
                    self.assertTrue(all(
                        line.startswith("echo ") for line in lines[select_at + 1:link_at]
                    ), lines[select_at:link_at + 1])
                    self.assertIn(" %TCC_OPENLIBM% ", links[0])

    def test_msvc_fallbacks_build_the_next_stage(self):
        attempts = re.findall(
            r"call :build_msvc_stage (tcc|msvc|clang|gcc)\b", routine("msvc_strap")
        )
        self.assertEqual(attempts, ["tcc", "msvc", "clang", "gcc"])

    def test_clang_generation_matches_the_stage_compiler(self):
        commands = routine("build_stage_with_clang").splitlines()
        configure = next(line for line in commands if line.startswith("set stage_vflags="))
        compile_c = next(line for line in commands if line.lstrip().startswith('"!clang_exe!" '))
        self.assertIn('!clang_vflags!', configure)
        self.assertIn('if !clang_msvc! EQU 1 set stage_vflags=-cc msvc', routine('build_stage_with_clang'))
        self.assertNotIn('--target=', compile_c)
        self.assertNotIn('--target=', SOURCE)
        self.assertIn('-fwrapv', compile_c)
        self.assertNotIn("%VC_BOOTSTRAP_DEFINE%", compile_c)

    def test_clang_uses_its_installed_target_and_msvc_driver_when_needed(self):
        bootstrap = routine("build_bootstrap_with_clang")
        self.assertIn("-dumpmachine", bootstrap)
        self.assertIn("call :configure_clang_target", bootstrap)
        self.assertNotIn("too old", bootstrap)
        config = routine("configure_clang_target")
        self.assertIn('!clang_target:msvc=!', config)
        self.assertIn('clang-cl.exe', config)
        self.assertIn('set clang_vflags=-cc "!clang_cl_exe!"', config)
        for label in ("build_bootstrap_with_clang", "build_stage_with_clang"):
            with self.subTest(label=label):
                command = next(line.strip() for line in routine(label).splitlines()
                               if line.lstrip().startswith('"!clang_cl_exe!" '))
                self.assertIn('/std:c11', command)
                self.assertIn('/link /STACK:33554432', command)
                self.assertNotIn('-municode', command)
        self.assertIn('set updated_vflags=!clang_vflags!', routine('clang_strap'))

    def test_directly_linked_stage_does_not_require_gc(self):
        generate = next(
            line for line in routine("generate_stage_c").splitlines()
            if line.lstrip().startswith('"%V_BOOTSTRAP%" ')
        )
        self.assertIn("-gc none", generate)

    def test_bootstrap_only_emits_c_and_never_spawns_a_stage_compiler(self):
        commands = [
            line for line in SOURCE.splitlines()
            if line.lstrip().startswith('"%V_BOOTSTRAP%" ')
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
                link = next(line for line in body.splitlines() if line.lstrip().startswith(prefix))
                self.assertIn('"%V_STAGE_C%"', link)
                self.assertIn('"%V_STAGE%"', link)
                self.assertNotIn("%VC_BOOTSTRAP_DEFINE%", link)
                self.assertIn('set stage_error=!ERRORLEVEL!\ncall :try_delete "%V_STAGE_C%"', body)
                self.assertIn("exit /b !stage_error!", body)

    def test_all_routes_use_the_fresh_stage_to_build_the_installed_compiler(self):
        for label, stage in (
            ("build_fresh_v_with_tcc", "tcc"),
            ("clang_strap", "clang"),
            ("gcc_strap", "gcc"),
            ("msvc_strap", "tcc"),
        ):
            with self.subTest(label=label):
                body = routine(label)
                if label == "msvc_strap":
                    self.assertIn("call :build_msvc_stage " + stage, body)
                    self.assertIn("call :build_stage_with_%~1", routine("build_msvc_stage"))
                else:
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

    def test_targets_are_selected_without_their_quotes(self):
        # `v up` runs `os.exec(['makev.bat', 'latest_tcc'])`, and `os.exec`
        # quotes every argument on Windows, so `%1` is `"latest_tcc"` there.
        self.assertIn("set target=%~1&", routine("verifyopt"))
        self.assertNotIn("set target=%1&", SOURCE)


@unittest.skipUnless(os.name == "nt", "requires the Windows cmd.exe interpreter")
class BatchExecutionTests(unittest.TestCase):
    def test_clang_queries_a_compiler_path_with_spaces(self):
        with tempfile.TemporaryDirectory(prefix="makev clang discovery ") as directory:
            workdir = Path(directory)
            clang = workdir / "LLVM with spaces" / "clang.cmd"
            clang.parent.mkdir()
            clang.write_text('@echo off\necho x86_64-w64-windows-gnu\n', encoding="utf-8")
            lines = routine("build_bootstrap_with_clang").splitlines()
            for index, line in enumerate(lines):
                if line.lstrip().startswith(('"!clang_exe!" ', '"!clang_cl_exe!" ')):
                    lines[index] = "call :stub_compile"
            script = "\n".join([
                "@echo off", "setlocal EnableExtensions EnableDelayedExpansion",
                'set "test_clang=' + str(clang) + '"',
                "call :build_bootstrap_with_clang",
                "set test_status=!ERRORLEVEL!",
                "echo target=!clang_target!", "echo mode=!clang_msvc!",
                "exit /b !test_status!", "\n".join(lines), routine("configure_clang_target"),
                ":resolve_executable", 'set "resolved_exe=!test_clang!"', "exit /b 0",
                ":stub_compile", "exit /b 0",
            ])
            harness = workdir / "discovery harness.bat"
            harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
            result = subprocess.run(
                [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
                cwd=workdir, capture_output=True, text=True, errors="replace",
                timeout=15, check=False,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("target=x86_64-w64-windows-gnu", result.stdout)
            self.assertIn("mode=0", result.stdout)

    def test_clang_target_configuration_preserves_the_installed_runtime(self):
        with tempfile.TemporaryDirectory(prefix="makev clang target ") as directory:
            workdir = Path(directory)
            clang = workdir / "clang.exe"
            clang_cl = workdir / "clang-cl.exe"
            for target, has_clang_cl, expected_status, expected_mode in (
                ("x86_64-w64-windows-gnu", False, 0, "0"),
                ("i686-w64-mingw32", False, 0, "0"),
                ("x86_64-pc-windows-msvc", True, 0, "1"),
                ("x86_64-pc-windows-msvc", False, 1, None),
            ):
                with self.subTest(target=target, clang_cl=has_clang_cl):
                    if has_clang_cl:
                        clang_cl.touch()
                    else:
                        clang_cl.unlink(missing_ok=True)
                    script = "\n".join([
                        "@echo off", "setlocal EnableExtensions EnableDelayedExpansion",
                        'set "clang_exe=' + str(clang) + '"',
                        'set "clang_target=' + target + '"',
                        "call :configure_clang_target",
                        "set target_status=!ERRORLEVEL!",
                        "echo mode=!clang_msvc!",
                        "echo flags=!clang_vflags!",
                        "exit /b !target_status!",
                        routine("configure_clang_target"),
                    ])
                    harness = workdir / "target harness.bat"
                    harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
                    result = subprocess.run(
                        [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
                        cwd=workdir, capture_output=True, text=True, errors="replace",
                        timeout=15, check=False,
                    )
                    self.assertEqual(result.returncode, expected_status, result.stdout + result.stderr)
                    if expected_mode is not None:
                        self.assertIn("mode=" + expected_mode, result.stdout)
                        self.assertIn(str(clang_cl if expected_mode == "1" else clang), result.stdout)
                        self.assertNotIn("--target", result.stdout)

    def test_tcc_links_use_the_installed_bundle_openlibm(self):
        # Updating an existing checkout does not switch its branch, and --local
        # skips the update. The host, -tcc32 and TCC_BRANCH must not matter:
        # every link uses the object that the installed bundle ships, if any.
        for arch, options, branch, bootstrap_object, stage_object in (
            ("AMD64", [], None, True, True),
            ("AMD64", [], None, False, False),
            ("AMD64", ["--local"], None, True, True),
            ("AMD64", ["--local"], None, False, False),
            ("x86", [], None, True, True),
            ("AMD64", ["-tcc32"], None, True, True),
            ("AMD64", ["-tcc32", "--local"], None, False, False),
            ("AMD64", [], "thirdparty-windows-i386", True, True),
            ("AMD64", ["--local"], '"thirdparty-windows-i386"', True, True),
            ("x86", [], "thirdparty-windows-amd64", False, False),
            ("AMD64", [], "custom-tcc-bundle", False, False),
            ("AMD64", [], None, True, False),
            ("AMD64", [], None, False, True),
        ):
            with self.subTest(
                arch=arch, options=options, branch=branch,
                bootstrap_object=bootstrap_object, stage_object=stage_object,
            ), tempfile.TemporaryDirectory(prefix="makev TCC link inputs ") as directory:
                workdir = Path(directory)
                tcc_dir = workdir / "thirdparty" / "tcc"
                openlibm = tcc_dir / "lib" / "openlibm.o"
                openlibm.parent.mkdir(parents=True)
                if bootstrap_object:
                    openlibm.write_text("bundle object", encoding="utf-8")
                # A real child batch file records each whole link command line;
                # only compiler work is stubbed, not the option or build flow.
                # It echoes %* because -B"<dir with spaces>" breaks an IF on %~1.
                compiler = tcc_dir / "TCC stub.cmd"
                compiler.write_bytes(
                    b'@echo off\r\n>>"%test_calls%" echo(%*\r\nexit /b 0\r\n'
                )
                preamble = SOURCE[:SOURCE.index(":verifyopt\n")]
                preamble = preamble.replace("%~dp0", str(workdir) + "\\")
                body = "\n".join(
                    routine("build_" + stage + "_with_tcc") for stage in ("bootstrap", "stage")
                )
                body, compiler_calls = re.subn(
                    r'(?m)^"!tcc_exe!" ', 'call "!tcc_exe!" ', body
                )
                self.assertEqual(compiler_calls, 2)
                script = "\n".join([
                    preamble,
                    'set "tcc_exe=' + str(compiler) + '"',
                    "goto :verifyopt",
                    routine("verifyopt"),
                    routine("init"),
                    routine("build"),
                    ":tcc_strap",
                    ":tcc32_strap",
                    "set test_calls=bootstrap.args",
                    "call :build_bootstrap_with_tcc",
                    "if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!",
                    # The installed bundle can gain or lose the object between links.
                    'if "%TCC_TEST_STAGE_OBJECT%" == "1" echo bundle object>"%tcc_dir%\\lib\\openlibm.o"',
                    'if not "%TCC_TEST_STAGE_OBJECT%" == "1" del "%tcc_dir%\\lib\\openlibm.o" >nul 2>&1',
                    "set test_calls=stage.args",
                    "call :build_stage_with_tcc",
                    "exit /b !ERRORLEVEL!",
                    body,
                    routine("select_tcc_openlibm"),
                    ":download_tcc",
                    '>>"download.txt" echo update existing checkout',
                    "exit /b 0",
                    ":cloning_vc",
                    "exit /b 0",
                    ":generate_stage_c",
                    "exit /b 0",
                    ":try_delete",
                    "exit /b 0",
                ])
                harness = workdir / "TCC link inputs.bat"
                harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
                env = dict(os.environ, PROCESSOR_ARCHITECTURE=arch, TCC_OPENLIBM="stale value",
                           TCC_TEST_STAGE_OBJECT="1" if stage_object else "0")
                env.pop("TCC_BRANCH", None)
                if branch is not None:
                    env["TCC_BRANCH"] = branch
                command = [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)]
                command.extend(options)
                result = subprocess.run(
                    command, cwd=workdir, env=env, capture_output=True, text=True,
                    errors="replace", timeout=15, check=False,
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual((workdir / "download.txt").exists(), "--local" not in options)
                for stage, present in (("bootstrap", bootstrap_object), ("stage", stage_object)):
                    links = (workdir / (stage + ".args")).read_text(encoding="utf-8").splitlines()
                    self.assertEqual(len(links), 1, links)
                    link = links[0]
                    self.assertEqual(link.lower().count("openlibm.o"), 1 if present else 0, link)
                    if present:
                        self.assertIn(' "' + str(openlibm) + '" ', link)
                    self.assertIn("-Wl,-stack=33554432", link)
                    self.assertNotIn("stale value", link)

    def test_tcc_bootstrap_links_external_helpers_without_an_existing_compiler(self):
        if not (ROOT / "thirdparty" / "tcc" / "tcc.exe").is_file():
            self.skipTest("requires the bundled Windows TCC")
        with tempfile.TemporaryDirectory(prefix="makev native helpers ") as directory:
            workdir = Path(directory)
            snapshot = workdir / "v_win.c"
            # Model the declaration-only helpers in current vc snapshots. Older
            # snapshots embedded static definitions and hid missing link inputs.
            snapshot_source = r"""
#include <stddef.h>
void *v3_win_thread_create(size_t, void *(*)(void *), void *, unsigned int *);
int v3_win_thread_is_current(unsigned int);
int v3_win_thread_join(void *);
unsigned long long v_time_ticks_ms(void);
int v_toolcache_close_handle(void *);
void *v_toolcache_create_file_w(const unsigned short *, unsigned int,
    unsigned int, unsigned int, unsigned int);
int v_toolcache_file_is_disk(void *);
int v_toolcache_get_file_information(void *, void *);
int v_toolcache_move_file_ex_w(const unsigned short *, const unsigned short *, unsigned int);
int v_toolcache_read_file(void *, void *, unsigned int, unsigned int *);
int v_toolcache_root_is_private(const unsigned short *);

/* Relocations keep every helper referenced, even with an optimizing compiler. */
void (*const helpers[])(void) = {
    (void (*)(void))v3_win_thread_create,
    (void (*)(void))v3_win_thread_is_current,
    (void (*)(void))v3_win_thread_join,
    (void (*)(void))v_time_ticks_ms,
    (void (*)(void))v_toolcache_close_handle,
    (void (*)(void))v_toolcache_create_file_w,
    (void (*)(void))v_toolcache_file_is_disk,
    (void (*)(void))v_toolcache_get_file_information,
    (void (*)(void))v_toolcache_move_file_ex_w,
    (void (*)(void))v_toolcache_read_file,
    (void (*)(void))v_toolcache_root_is_private,
};
int main(void) {
    for (size_t i = 0; i < sizeof(helpers) / sizeof(helpers[0]); ++i) {
        if (helpers[i] == NULL) return 1;
    }
    return v_time_ticks_ms() == 0 || v3_win_thread_is_current(0);
}
"""
            # Use the actual configuration and bootstrap routine, including its
            # quoted native source paths, without invoking downloads or stages.
            preamble = SOURCE[:SOURCE.index(":verifyopt\n")]
            preamble = preamble.replace("%~dp0", str(ROOT) + "\\")
            bootstrap = workdir / "v_win_bootstrap.exe"
            script = "\n".join([
                preamble,
                'set "V_C_FILE=' + str(snapshot) + '"',
                'set "V_BOOTSTRAP=' + str(bootstrap) + '"',
                'set "V_EXE=' + str(workdir / "missing-v.exe") + '"',
                "call :build_bootstrap_with_tcc",
                "exit /b !ERRORLEVEL!",
                routine("build_bootstrap_with_tcc"),
                routine("select_tcc_openlibm"),
            ])
            harness = workdir / "bootstrap harness.bat"
            harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
            for embedded in (False, True):
                with self.subTest(embedded=embedded):
                    headers = ""
                    if embedded:
                        # Older snapshots include static helper definitions.
                        headers = "".join(
                            '#include "' + (ROOT / path).as_posix() + '"\n'
                            for path in (
                                "vlib/time/ticks_windows.h",
                                "vlib/v/winthread_helper.h",
                                "cmd/v/toolcache_windows_helpers.h",
                            )
                        )
                    snapshot.write_text(headers + snapshot_source, encoding="utf-8")
                    result = subprocess.run(
                        [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
                        cwd=workdir, capture_output=True, text=True, errors="replace",
                        timeout=60, check=False,
                    )
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    result = subprocess.run([str(bootstrap)], cwd=workdir, timeout=15, check=False)
                    self.assertEqual(result.returncode, 0)
            self.assertFalse((workdir / "missing-v.exe").exists())

    def run_msvc_bootstrap(self, workdir, bootstrap_statuses=None, generate_statuses=None,
                           link_statuses=None, final_status=0):
        bootstrap_statuses = bootstrap_statuses or {}
        generate_statuses = generate_statuses or {}
        link_statuses = link_statuses or {}
        msvc = routine("msvc_strap")
        # MSVC discovery and vsdevcmd.bat setup are outside the fallback contract.
        msvc = ":msvc_strap\n" + msvc[msvc.index("set ObjFile=.v.c.obj"):]
        labels = [
            "build_msvc_stage", "generate_stage_c", "build_stage_with_tcc",
            "build_stage_with_msvc", "build_stage_with_clang", "build_stage_with_gcc",
            "try_delete", "compile_error", "error", "select_tcc_openlibm",
        ]
        body = "\n".join([msvc] + [routine(label) for label in labels])
        lines = body.splitlines()
        replacements = {"generate": 0, "link": 0, "final": 0}
        for index, line in enumerate(lines):
            command = line.lstrip()
            if command.startswith('"%V_BOOTSTRAP%" '):
                lines[index] = command.replace('"%V_BOOTSTRAP%"', "call :stub_generate", 1)
                replacements["generate"] += 1
            elif command.startswith('"%V_STAGE%" '):
                lines[index] = "call :stub_final"
                replacements["final"] += 1
            elif command.startswith('"!clang_cl_exe!" '):
                lines[index] = "call :stub_link clang"
                replacements["link"] += 1
            elif command.startswith("ping 192.0.2.1 "):
                lines[index] = "rem No cleanup delay in tests"
            else:
                for compiler in ("tcc", "msvc", "clang", "gcc"):
                    prefix = "cl " if compiler == "msvc" else '"!' + compiler + '_exe!" '
                    if command.startswith(prefix):
                        lines[index] = "call :stub_link " + compiler
                        replacements["link"] += 1
        self.assertEqual(replacements, {"generate": 1, "link": 5, "final": 1})
        body = "\n".join(lines)
        calls = workdir / "bootstrap calls.txt"
        stubs = []
        for compiler in ("tcc", "msvc", "clang", "gcc"):
            expected_cc = "msvc" if compiler == "msvc" else str(workdir / (compiler + ".exe"))
            stubs.extend([
                ":build_bootstrap_with_" + compiler,
                'set "test_compiler=' + compiler + '"',
                'set "test_expected_cc=' + expected_cc + '"',
                '>>"%test_calls%" echo bootstrap ' + compiler,
                '>"%V_BOOTSTRAP%" echo bootstrap compiler',
                "exit /b " + str(bootstrap_statuses.get(compiler, 0)),
            ])
        stubs.extend([
            ":stub_generate",
            '>>"%test_calls%" echo generate !test_compiler!',
            'set "test_stage_cc="',
            'set "test_stage_output="',
            ":stub_generate_args",
            'if "%~1" == "" goto :stub_generate_ready',
            'if "%~1" == "-cc" set "test_stage_cc=%~2"',
            'if "%~1" == "-o" set "test_stage_output=%~2"',
            "shift",
            "goto :stub_generate_args",
            ":stub_generate_ready",
            'if not "!test_stage_cc!" == "!test_expected_cc!" exit /b 99',
            'if not "!test_stage_output!" == "%V_STAGE_C%" exit /b 98',
            '>"%V_STAGE_C%" echo partial stage C',
        ])
        for compiler in ("tcc", "msvc", "clang", "gcc"):
            stubs.append(
                'if "!test_compiler!" == "' + compiler + '" exit /b '
                + str(generate_statuses.get(compiler, 0))
            )
        stubs.extend([
            "exit /b 99",
            ":stub_link",
            '>>"%test_calls%" echo link %~1',
            'if not "%~1" == "!test_compiler!" exit /b 97',
            'if not exist "%V_STAGE_C%" exit /b 96',
            '>"%V_STAGE%" echo partial stage compiler',
        ])
        for compiler in ("tcc", "msvc", "clang", "gcc"):
            stubs.append(
                'if "!test_compiler!" == "' + compiler + '" exit /b '
                + str(link_statuses.get(compiler, 0))
            )
        stubs.extend([
            "exit /b 99",
            ":stub_final",
            '>>"%test_calls%" echo final msvc',
            'if not exist "%V_STAGE%" exit /b 95',
            '>"%V_UPDATED%" echo updated compiler',
            "exit /b " + str(final_status),
            ":move_updated_to_v",
            '>>"%test_calls%" echo install',
            "exit /b 0",
            ":success",
            "exit /b 0",
        ])
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            'set "test_calls=' + str(calls) + '"',
            'set "V_EXE=' + str(workdir / "v.exe") + '"',
            'set "V_UPDATED=' + str(workdir / "v_up.exe") + '"',
            'set "V_BOOTSTRAP=' + str(workdir / "v_win_bootstrap.exe") + '"',
            'set "V_STAGE=' + str(workdir / "v_stage.exe") + '"',
            'set "V_STAGE_C=' + str(workdir / "v_stage.c") + '"',
            'set "tcc_exe=' + str(workdir / "tcc.exe") + '"',
            'set "clang_exe=' + str(workdir / "clang.exe") + '"',
            'set "gcc_exe=' + str(workdir / "gcc.exe") + '"',
            'set "clang_target=x86_64-w64-mingw32"',
            'set clang_msvc=0',
            'set clang_vflags=-cc "!clang_exe!"',
            "call :msvc_strap",
            "exit /b !ERRORLEVEL!",
            body,
            "\n".join(stubs),
        ])
        harness = workdir / "msvc bootstrap harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        result = subprocess.run(
            [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
            cwd=workdir, capture_output=True, text=True, errors="replace",
            timeout=15, check=False,
        )
        return result, calls.read_text(encoding="utf-8").splitlines()

    def test_msvc_stops_after_the_first_working_bootstrap(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result, calls = self.run_msvc_bootstrap(Path(directory))
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(calls, [
                "bootstrap tcc", "generate tcc", "link tcc", "final msvc", "install",
            ])

    def test_msvc_retries_after_bootstrap_compile_generation_or_link_failure(self):
        for bootstrap_statuses, generate_statuses, link_statuses, first_calls in (
            ({"tcc": 47}, {}, {}, ["bootstrap tcc"]),
            ({}, {"tcc": 47}, {}, ["bootstrap tcc", "generate tcc"]),
            ({}, {}, {"tcc": 47}, ["bootstrap tcc", "generate tcc", "link tcc"]),
        ):
            with self.subTest(
                bootstrap=bootstrap_statuses, generate=generate_statuses, link=link_statuses
            ), tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
                result, calls = self.run_msvc_bootstrap(
                    Path(directory), bootstrap_statuses, generate_statuses, link_statuses
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(calls, first_calls + [
                    "bootstrap msvc", "generate msvc", "link msvc", "final msvc", "install",
                ])

    def test_msvc_retries_with_clang_after_two_generation_failures(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result, calls = self.run_msvc_bootstrap(
                Path(directory), generate_statuses={"tcc": 47, "msvc": 48}
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(calls, [
                "bootstrap tcc", "generate tcc", "bootstrap msvc", "generate msvc",
                "bootstrap clang", "generate clang", "link clang", "final msvc", "install",
            ])

    def test_msvc_retries_with_gcc_after_clang_generation_or_link_failure(self):
        for generate_status, link_status in ((49, 0), (0, 49)):
            with self.subTest(
                generate=generate_status, link=link_status
            ), tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
                result, calls = self.run_msvc_bootstrap(
                    Path(directory),
                    bootstrap_statuses={"msvc": 48},
                    generate_statuses={"tcc": 47, "clang": generate_status},
                    link_statuses={"clang": link_status},
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                expected = [
                    "bootstrap tcc", "generate tcc", "bootstrap msvc",
                    "bootstrap clang", "generate clang",
                ]
                if generate_status == 0:
                    expected.append("link clang")
                self.assertEqual(calls, expected + [
                    "bootstrap gcc", "generate gcc", "link gcc", "final msvc", "install",
                ])

    def test_msvc_exhausted_fallback_preserves_the_existing_compiler(self):
        failures = {"tcc": 47, "msvc": 48, "clang": 49, "gcc": 50}
        for phase in ("bootstrap", "generate", "link"):
            with self.subTest(phase=phase), tempfile.TemporaryDirectory(
                prefix="makev tests "
            ) as directory:
                workdir = Path(directory)
                (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
                result, calls = self.run_msvc_bootstrap(
                    workdir,
                    bootstrap_statuses=failures if phase == "bootstrap" else {},
                    generate_statuses=failures if phase == "generate" else {},
                    link_statuses=failures if phase == "link" else {},
                )
                self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
                expected = []
                for compiler in failures:
                    expected.append("bootstrap " + compiler)
                    if phase != "bootstrap":
                        expected.append("generate " + compiler)
                    if phase == "link":
                        expected.append("link " + compiler)
                self.assertEqual(calls, expected)
                self.assertEqual((workdir / "v.exe").read_text(), "working compiler")
                self.assertFalse((workdir / "v_stage.exe").exists())
                self.assertFalse((workdir / "v_stage.c").exists())

    def test_msvc_final_failure_is_not_masked_by_cleanup(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
            result, calls = self.run_msvc_bootstrap(workdir, final_status=51)
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(calls, [
                "bootstrap tcc", "generate tcc", "link tcc", "final msvc",
            ])
            self.assertEqual((workdir / "v.exe").read_text(), "working compiler")
            self.assertFalse((workdir / "v_stage.exe").exists())

    def run_stage(self, compiler, workdir, generate_status=0, link_status=0, final_status=0):
        labels = ["generate_stage_c", "build_stage_with_" + compiler, "try_delete"]
        if compiler == "tcc":
            labels.append("build_fresh_v_with_tcc")
        body = "\n".join(routine(label) for label in labels)
        body = body.replace('"%V_BOOTSTRAP%" ', "call :stub_generate ")
        prefix = "cl " if compiler == "msvc" else '"!' + compiler + '_exe!" '
        body = body.replace(prefix, "call :stub_link ")
        body = body.replace('"!clang_cl_exe!" ', "call :stub_link ")
        body = body.replace('"%V_STAGE%" ', "call :stub_final ")
        if compiler == "tcc":
            body += "\n" + routine("select_tcc_openlibm")
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
            "set clang_msvc=0",
            'set clang_vflags=-cc "!clang_exe!"',
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
            "smoke_version": '"%V_UPDATED%" version',
            "smoke_compile": '"%V_UPDATED%" !updated_vflags! -o "%V_UPDATED%.smoke.exe" examples/hello_world.v '
            '> "%V_UPDATED%.smoke.log" 2>&1',
            "smoke_run": '"%V_UPDATED%.smoke.exe" > nul',
        }
        body = "\n".join(
            routine(name) for name in (
                "check", "success", "version", "smoke_test_updated", "smoke_log_is_clean",
                "try_delete", "move_updated_to_v",
            )
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

    def run_self_hosted(self, workdir, status=0, writes=False, compiler_present=True):
        compiler = workdir / "v.exe"
        if compiler_present:
            compiler.write_text("placeholder compiler", encoding="utf-8")
        else:
            compiler = workdir / "missing v.exe"
        lines = routine("build_self_hosted_with_tcc").splitlines()
        replaced = 0
        for index, line in enumerate(lines):
            if line.startswith('"%V_EXE%" '):
                lines[index] = line.replace('"%V_EXE%"', "call :stub_v", 1)
                replaced += 1
        self.assertEqual(replaced, 1)
        try_delete = routine("try_delete")
        for wait in ("ping 192.0.2.1 -n 1 -w 100 >nul", "ping 192.0.2.1 -n 1 -w 250 >nul"):
            try_delete = try_delete.replace(wait, "rem No delay in tests")
        stub = [":stub_v", '>>"%test_calls%" echo %*']
        if writes:
            stub.append('>"%V_UPDATED%" echo built')
        stub.append("exit /b " + str(status))
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            'set "V_EXE=' + str(compiler) + '"',
            'set "V_UPDATED=' + str(workdir / "v_up.exe") + '"',
            "set V_BOOTSTRAP_VFLAGS=-no-parallel",
            'set "tcc_exe=C:\\fake tools\\tcc.exe"',
            'set "test_calls=' + str(workdir / "stub calls.txt") + '"',
            "call :build_self_hosted_with_tcc",
            "exit /b !ERRORLEVEL!",
            "\n".join(lines),
            try_delete,
            "\n".join(stub),
        ])
        harness = workdir / "self hosted harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        result = subprocess.run(
            [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
            cwd=workdir,
            capture_output=True,
            text=True,
            errors="replace",
            timeout=15,
            check=False,
        )
        calls = workdir / "stub calls.txt"
        return result, (calls.read_text(encoding="utf-8").splitlines() if calls.exists() else [])

    def test_self_hosted_build_installs_only_a_successful_compile(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            result, calls = self.run_self_hosted(workdir, writes=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual((workdir / "v_up.exe").read_text().strip(), "built")
            self.assertEqual(len(calls), 1)
            self.assertIn("-no-parallel -g -cc", calls[0])
            self.assertIn("cmd/v", calls[0])

    def test_self_hosted_build_failure_removes_partial_output_and_keeps_exit_code(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            result, calls = self.run_self_hosted(workdir, status=47, writes=True)
            self.assertEqual(result.returncode, 47, result.stdout + result.stderr)
            self.assertFalse((workdir / "v_up.exe").exists())
            self.assertEqual(len(calls), 1)

    def test_self_hosted_build_reports_a_missing_compiler_without_running_anything(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            result, calls = self.run_self_hosted(workdir, compiler_present=False)
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            self.assertIn("Self-hosted compiler not found", result.stdout)
            self.assertEqual(calls, [])
            self.assertFalse((workdir / "v_up.exe").exists())

    def run_tcc_routing(self, workdir, bootstrap=0, fresh=0, self_hosted=0, clang=1, gcc=1,
                        install=0):
        # Runs the real TCC route, from `tcc_strap` through `tcc_retry_with_host_bootstrap`,
        # with every compiler step stubbed to record its name and return the given status.
        start = SOURCE.index("\n:tcc_strap\n") + 1
        end = SOURCE.index("\n:build_fresh_v_with_tcc\n") + 1
        stubs = []
        for label, record, status in (
            ("build_bootstrap_with_tcc", "bootstrap tcc", bootstrap),
            ("build_fresh_v_with_tcc", "fresh", fresh),
            ("build_self_hosted_with_tcc", "self-hosted", self_hosted),
            ("build_bootstrap_with_clang", "bootstrap clang", clang),
            ("build_bootstrap_with_gcc", "bootstrap gcc", gcc),
            ("clang_strap", "clang_strap", 1),
            ("move_updated_to_v", "install", install),
            ("compile_error", "compile_error", 1),
            ("success", "success", 0),
        ):
            stubs.extend([
                ":" + label, '>>"%test_calls%" echo ' + record, "exit /b " + str(status),
            ])
        calls = workdir / "routing calls.txt"
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            "set compiler=",
            'set "V_EXE=v.exe"',
            'set "V_UPDATED=v_up.exe"',
            'set "test_calls=' + str(calls) + '"',
            "call :tcc_strap",
            "exit /b !ERRORLEVEL!",
            SOURCE[start:end],
            "\n".join(stubs),
        ])
        harness = workdir / "routing harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        result = subprocess.run(
            [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
            cwd=workdir,
            capture_output=True,
            text=True,
            errors="replace",
            timeout=15,
            check=False,
        )
        return result, (calls.read_text(encoding="utf-8").splitlines() if calls.exists() else [])

    def test_bootstrap_failure_with_a_failed_self_hosted_build_stops_without_repeat_work(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result, calls = self.run_tcc_routing(Path(directory), bootstrap=1, self_hosted=1)
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            self.assertEqual(calls, ["bootstrap tcc", "self-hosted", "compile_error"])

    def test_bootstrap_failure_installs_a_successful_self_hosted_build(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result, calls = self.run_tcc_routing(Path(directory), bootstrap=1, self_hosted=0)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(calls, ["bootstrap tcc", "self-hosted", "install", "success"])

    def test_stage_failure_keeps_the_host_bootstrap_retry_after_a_failed_self_hosted_build(self):
        # Stage failures keep their existing retry chain. Only a failed bootstrap skips it.
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result, calls = self.run_tcc_routing(Path(directory), fresh=1, self_hosted=1)
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            self.assertEqual(calls, [
                "bootstrap tcc", "fresh", "self-hosted", "bootstrap clang", "bootstrap gcc",
                "clang_strap",
            ])

    def test_stage_failure_installs_a_successful_self_hosted_build(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            result, calls = self.run_tcc_routing(Path(directory), fresh=1, self_hosted=0)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(calls, [
                "bootstrap tcc", "fresh", "self-hosted", "install", "success",
            ])

    def run_smoke_log_check(self, workdir, log_text):
        log = workdir / "smoke.log"
        log.write_text(log_text, encoding="utf-8")
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            'call :smoke_log_is_clean "' + str(log) + '"',
            "exit /b !ERRORLEVEL!",
            routine("smoke_log_is_clean"),
        ])
        harness = workdir / "smoke log harness.bat"
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

    def test_smoke_log_reports_a_compiler_fallback(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            clean = self.run_smoke_log_check(workdir, "Hello, World!\n")
            self.assertEqual(clean.returncode, 0, clean.stdout + clean.stderr)
            for log_text in (
                "warning: tcc compilation failed, falling back to gcc.exe\n",
                "V compilation failed (c_compilation_error); retrying with the V1 compiler\n",
                "note: V could not build this program with the default compiler\n",
            ):
                with self.subTest(log=log_text):
                    result = self.run_smoke_log_check(workdir, log_text)
                    self.assertEqual(result.returncode, 1, result.stdout + result.stderr)

    def test_failed_smoke_version_keeps_the_existing_compiler(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            workdir = Path(directory)
            (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
            (workdir / "v_up.exe").write_text("new compiler", encoding="utf-8")
            result = self.run_batch("move_updated_to_v", workdir, {"smoke_version": 37})
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual((workdir / "v.exe").read_text(), "working compiler")
            self.assertEqual((workdir / "v_up.exe").read_text(), "new compiler")
            self.assertFalse((workdir / "v_old.exe").exists())

    def test_failed_smoke_compile_or_run_keeps_the_existing_compiler(self):
        for step, status in (("smoke_compile", 1), ("smoke_run", 37)):
            with self.subTest(step=step), tempfile.TemporaryDirectory(
                prefix="makev tests "
            ) as directory:
                workdir = Path(directory)
                (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
                (workdir / "v_up.exe").write_text("new compiler", encoding="utf-8")
                result = self.run_batch("move_updated_to_v", workdir, {step: status})
                self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual((workdir / "v.exe").read_text(), "working compiler")
                self.assertFalse((workdir / "v_old.exe").exists())

    def run_install(self, workdir, path, clang_target="x86_64-w64-mingw32"):
        # Runs one install route from the build of "%V_UPDATED%" through
        # :move_updated_to_v, with every compiler stubbed. The stub for the new
        # compiler builds hello world only with the toolchain that built it, like
        # a `makev.bat -msvc --local` clone with no bundled TCC and no gcc, where
        # the driver has no usable default C compiler (vlang/v#30278 review).
        stubbed = {"compile_error": 1, "error": 1, "success": 0}
        if path == "msvc":
            body = routine("msvc_strap")
            # MSVC discovery and vsdevcmd.bat setup are outside this contract.
            body = ":msvc_strap\n" + body[body.index("set ObjFile=.v.c.obj"):]
            entry = "msvc_strap"
            stubbed["build_msvc_stage"] = 0
        elif path == "tcc":
            body = routine("tcc32_strap") + routine("build_fresh_v_with_tcc")
            entry = "tcc32_strap"
            stubbed.update({"build_bootstrap_with_tcc": 0, "build_stage_with_tcc": 0})
        elif path == "self_hosted":
            body = routine("bootstrap_self_hosted_fallback") + routine("build_self_hosted_with_tcc")
            entry = "bootstrap_self_hosted_fallback"
        else:
            body = routine(path + "_strap")
            entry = path + "_strap"
            stubbed.update({
                "build_bootstrap_with_" + path: 0, "build_stage_with_" + path: 0,
            })
        body += "\n".join(
            routine(name) for name in (
                "move_updated_to_v", "smoke_test_updated", "smoke_log_is_clean", "try_delete",
            )
        )
        lines = body.splitlines()
        replaced = {"build": 0, "updated": 0, "run": 0}
        for index, line in enumerate(lines):
            command = line.lstrip()
            if command.startswith('"%V_STAGE%" ') or command.startswith('"%V_EXE%" '):
                lines[index] = "call :stub_build " + command.split(" ", 1)[1]
                replaced["build"] += 1
            elif command.startswith('"%V_UPDATED%" '):
                lines[index] = "call :stub_updated " + command.split(" ", 1)[1]
                replaced["updated"] += 1
            elif command == '"%V_UPDATED%.smoke.exe" > nul':
                lines[index] = "call :stub_run"
                replaced["run"] += 1
            elif command.startswith("ping 192.0.2.1 "):
                lines[index] = "rem No delay in tests"
        self.assertEqual(replaced, {"build": 1, "updated": 2, "run": 1})
        stubs = []
        for label, status in stubbed.items():
            stubs.extend([":" + label, '>>"%test_calls%" echo ' + label])
            if label == "build_bootstrap_with_clang":
                # Keep target discovery's configuration side effects while
                # stubbing the C compilation and executable lookup.
                stubs.extend(["call :configure_clang_target", "exit /b !ERRORLEVEL!"])
            else:
                stubs.append("exit /b " + str(status))
        if path == "clang":
            stubs.append(routine("configure_clang_target"))
            if "msvc" in clang_target:
                (workdir / "clang-cl.exe").touch()
        for stub in ("build", "updated"):
            stubs.extend([
                ":stub_" + stub,
                'if "%~1" == "version" exit /b 0',
                'set "test_' + stub + '_cc="',
                'set "test_' + stub + '_cflags="',
                ":stub_" + stub + "_args",
                'if "%~1" == "" goto :stub_' + stub + "_ready",
                'if "%~1" == "-cc" set "test_' + stub + '_cc=%~2"',
                'if "%~1" == "-cflags" set "test_' + stub + '_cflags=%~2"',
                "shift",
                "goto :stub_" + stub + "_args",
                ":stub_" + stub + "_ready",
                '>>"%test_calls%" echo ' + stub + " !test_" + stub + "_cc! !test_" + stub + "_cflags!",
            ])
            if stub == "build":
                stubs.extend(['>"%V_UPDATED%" echo updated compiler', "exit /b 0"])
            else:
                stubs.extend([
                    # No default C compiler: only the toolchain that built the compiler works.
                    'if "!test_updated_cc!" == "" exit /b 1',
                    'if not "!test_updated_cc!" == "!test_build_cc!" exit /b 94',
                    'if not "!test_updated_cflags!" == "!test_build_cflags!" exit /b 93',
                    '>"%V_UPDATED%.smoke.exe" echo hello world',
                    "exit /b 0",
                ])
        stubs.extend([":stub_run", '>>"%test_calls%" echo run', "exit /b 0"])
        calls = workdir / "install calls.txt"
        (workdir / "v.exe").write_text("working compiler", encoding="utf-8")
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            'set "test_calls=' + str(calls) + '"',
            'set "compiler=' + path + '"',
            'set "V_EXE=' + str(workdir / "v.exe") + '"',
            'set "V_UPDATED=' + str(workdir / "v_up.exe") + '"',
            'set "V_OLD=' + str(workdir / "v_old.exe") + '"',
            'set "V_STAGE=' + str(workdir / "v_stage.exe") + '"',
            "set V_BOOTSTRAP_VFLAGS=-no-parallel",
            'set "tcc_exe=' + str(workdir / "tcc.exe") + '"',
            'set "clang_exe=' + str(workdir / "clang.exe") + '"',
            'set "gcc_exe=' + str(workdir / "gcc.exe") + '"',
            'set "clang_target=' + clang_target + '"',
            "call :" + entry,
            "exit /b !ERRORLEVEL!",
            "\n".join(lines),
            "\n".join(stubs),
        ])
        harness = workdir / "install harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        result = subprocess.run(
            [os.environ.get("ComSpec", "cmd.exe"), "/d", "/c", str(harness)],
            cwd=workdir,
            capture_output=True,
            text=True,
            errors="replace",
            timeout=15,
            check=False,
        )
        recorded = calls.read_text(encoding="utf-8").splitlines() if calls.exists() else []
        return result, [line.strip() for line in recorded]

    def test_install_smoke_tests_with_the_toolchain_that_built_the_compiler(self):
        for path, cc, clang_target in (
            ("tcc", "tcc.exe", ""),
            ("self_hosted", "tcc.exe", ""),
            ("clang", "clang.exe", "x86_64-w64-mingw32"),
            ("clang", "clang-cl.exe", "x86_64-pc-windows-msvc"),
            ("gcc", "gcc.exe", ""),
            ("msvc", "msvc", ""),
        ):
            with self.subTest(path=path, target=clang_target), tempfile.TemporaryDirectory(
                prefix="makev tests "
            ) as directory:
                workdir = Path(directory)
                result, calls = self.run_install(workdir, path, clang_target)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                toolchain = cc if cc == "msvc" else str(workdir / cc)
                self.assertEqual(
                    [call for call in calls if not call.startswith("build_")],
                    ["build " + toolchain.strip(), "updated " + toolchain.strip(), "run", "success"],
                )
                self.assertEqual((workdir / "v.exe").read_text().strip(), "updated compiler")
                self.assertEqual((workdir / "v_old.exe").read_text(), "working compiler")

    def run_option_parser(self, workdir, arguments):
        targets = ("build", "clean", "cleanall", "check", "help", "latest_tcc", "rebuild")
        script = "\n".join([
            "@echo off",
            "setlocal EnableExtensions EnableDelayedExpansion",
            "set /a shift_counter=0",
            "set /a flag_local=0",
            "set compiler=",
            "set subcmd=",
            "set target=build",
            routine("verifyopt"),
            routine("init"),
            *(
                ":" + target + "\necho target=" + target + " compiler=!compiler!\nexit /b 0"
                for target in targets
            ),
        ])
        harness = workdir / "makev harness.bat"
        harness.write_bytes(script.replace("\n", "\r\n").encode("utf-8"))
        comspec = os.environ.get("ComSpec", "cmd.exe")
        # Pass a raw command line, so quoted arguments reach the batch file
        # exactly as `os.exec` sends them.
        return subprocess.run(
            '"' + comspec + '" /d /c ""' + str(harness) + '" ' + arguments + '"',
            cwd=workdir,
            capture_output=True,
            text=True,
            errors="replace",
            timeout=15,
            check=False,
        )

    def test_quoted_arguments_select_their_target(self):
        with tempfile.TemporaryDirectory(prefix="makev tests ") as directory:
            for arguments, expected in (
                ("latest_tcc", "target=latest_tcc compiler="),
                ('"latest_tcc"', "target=latest_tcc compiler="),
                ('"build" "-msvc"', "target=build compiler=msvc"),
            ):
                with self.subTest(arguments=arguments):
                    result = self.run_option_parser(Path(directory), arguments)
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    self.assertEqual(result.stdout.strip(), expected)


if __name__ == "__main__":
    unittest.main(verbosity=2)
