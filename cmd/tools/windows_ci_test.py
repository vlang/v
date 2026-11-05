"""Check Windows CI pins and execute its build-step failure paths without building V.

Run from any directory with: python cmd/tools/windows_ci_test.py
The native process fixtures require GCC and PowerShell on Windows.
"""

import hashlib
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / '.github' / 'workflows'
COMPILERS = ('gcc', 'msvc', 'tcc')
RUNNER = 'run_multiwindow_win32_w3_green.ps1'
EXIT_CHECK = 'if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }'
W5_RUNNER = 'run_multiwindow_win32_w5_a1.ps1'


def workflow_sources():
    return {cc: (WORKFLOWS / f'windows_ci_{cc}.yml').read_text(encoding='utf-8')
            for cc in COMPILERS}


def build_scripts(workflows):
    # These workflows use literal run blocks at the same indentation. Fail if
    # that layout changes instead of silently losing build-step coverage.
    blocks = re.compile(r'^        run: \|\n((?:(?:          [^\n]*)?\n)+)', re.M)
    result = []
    for cc, workflow in workflows.items():
        scripts = []
        for match in blocks.finditer(workflow):
            script = '\n'.join(line[10:] for line in match[1].splitlines()) + '\n'
            if re.search(r'^\.\\makev\.bat -' + cc + r'$', script, re.M):
                scripts.append(script)
        if len(scripts) != 2:
            raise ValueError(f'{cc}: expected both Windows build steps, got {len(scripts)}')
        result.extend((cc, script) for script in scripts)
    return result


def manifest_rows(script, name, keys):
    match = re.search(r'^\$' + name + r' = @\(\n(.*?)^\)\n', script, re.M | re.S)
    if match is None:
        raise ValueError(f'missing W3 {name} array')
    rows = re.findall(r'\[pscustomobject\]@\{(.*?)\}', match[1], re.S)
    remainder = re.sub(r'\[pscustomobject\]@\{.*?\}', '', match[1], flags=re.S)
    if not rows or remainder.strip():
        raise ValueError(f'invalid W3 {name} array')
    pattern = r'\s*' + r'\s+'.join(key + r" = '([^']+)'" for key in keys) + r'\s*'
    result = []
    for row in rows:
        fields = re.fullmatch(pattern, row)
        if fields is None:
            raise ValueError(f'invalid W3 {name} row')
        result.append(fields.groups())
    if len(result) != len(set(result)):
        raise ValueError(f'duplicate W3 {name} row')
    return result


def validate_w3_metadata(runner, workflows):
    script = runner.decode('utf-8')
    surface = manifest_rows(script, 'surface', ('Path', 'Hash'))
    cases = manifest_rows(script, 'cases', ('Kind', 'File', 'Name'))
    known = re.search(r"^\$knownCompositeSha256 = '([0-9a-f]{64})'$", script, re.M)
    if known is None:
        raise ValueError('missing W3 composite pin')
    records = ['schema=package2-win32-w3-green-surface-v1']
    records += [f'file={path}|sha256={digest}' for path, digest in surface]
    records += [f'case={kind}|{path}|{name}' for kind, path, name in cases]
    composite = hashlib.sha256(('\n'.join(records) + '\n').encode('utf-8')).hexdigest()
    if composite != known[1]:
        raise ValueError(f'W3 composite mismatch: expected={known[1]} actual={composite}')
    runner_hash = hashlib.sha256(runner).hexdigest()
    for cc in COMPILERS:
        calls = re.findall(
            re.escape(RUNNER) + r" -Compiler (\w+) `\n\s*"
            r"-ExpectedCompositeSha256 '([0-9a-f]{64})' `\n\s*"
            r"-ExpectedRunnerSha256 '([0-9a-f]{64})'", workflows[cc])
        if calls != [(cc, composite, runner_hash)]:
            raise ValueError(f'{cc}: W3 invocation or pins do not match the runner')
    return surface, cases


def validate_file_hash(path, expected):
    actual = hashlib.sha256(path.read_bytes()).hexdigest()
    if actual != expected:
        raise ValueError(f'{path}: expected={expected} actual={actual}')


def w5_metadata(script):
    def value(name):
        match = re.search(r'^\$' + name + r" = '([^']+)'$", script, re.M)
        if match is None:
            raise ValueError(f'missing W5 {name}')
        return match[1]

    def digest(records):
        return hashlib.sha256(('\n'.join(records) + '\n').encode('utf-8')).hexdigest()

    pins = {}
    files = {}
    for name, pin in (('testFile', 'Test'), ('oracle', 'Oracle'), ('a0Header', 'A0Header'),
                      ('a0Main', 'A0Main'), ('a0Runner', 'A0Runner')):
        path, expected = value(name), value('known' + pin + 'Sha256')
        files[path] = expected
        pins['Expected' + pin + 'Sha256'] = expected
    pins['ExpectedA0TupleSha256'] = value('knownA0TupleSha256')
    for array, schema, state, composite in (
        ('publicSurface', 'public-routing-surface', 'state=frozen', 'PublicRoutingSurface'),
        ('greenProductionSurface', 'production-surface', 'expectation=Green',
         'GreenProductionSurface'),
    ):
        body = re.search(r'^\$' + array + r' = @\(\n(.*?)^\)', script, re.M | re.S)
        if body is None:
            raise ValueError(f'missing W5 {array}')
        entries = re.findall(r"Path = '([^']+)'; Known = '([0-9a-f]{64})'; Passed = \$(\w+)",
                             body[1])
        if not entries:
            raise ValueError(f'empty W5 {array}')
        records = [f'schema=package2-win32-w5-a1-{schema}-v1', state]
        for path, expected, parameter in entries:
            files[path] = expected
            pins[parameter] = expected
            records.append(f'file={path}|sha256={expected}')
        actual = digest(records)
        if actual != value('known' + composite + 'Sha256'):
            raise ValueError(f'W5 {array} composite mismatch')
        pins['Expected' + composite + 'Sha256'] = actual

    test, oracle, case = value('testFile'), value('oracle'), value('caseName')
    records = [
        'schema=package2-win32-w5-a1-v1',
        f'selection=target|kind=absolute-explicit|path={test}',
        'selection=exclude|kind=absolute-testdata-wildcard|pattern=vlib/gg/testdata/*.v',
        f'selection=direct-child|path={test}',
        f'file={test}|sha256={files[test]}',
        f'file={oracle}|sha256={files[oracle]}',
        f'case=noflag|define=disabled|run_only={case}|expectation=BehavioralGreen|summary=1_passed_1_total|package_markers=0',
        f'case=enabled|define=gg_multiwindow|run_only={case}|expectation=BehavioralGreen|summary=1_passed_1_total',
    ]
    for state in ('red', 'green'):
        body = re.search(r'^\$' + state + r'Markers = @\(\n(.*?)^\)', script, re.M | re.S)[1]
        records += [f'marker={state}|{marker}' for marker in re.findall(r"'([^']+)'", body)]
    actual = digest(records)
    if actual != value('knownTupleSha256'):
        raise ValueError('W5 test tuple mismatch')
    pins['ExpectedTupleSha256'] = actual
    pins['ExpectedRunnerSha256'] = hashlib.sha256(script.encode('utf-8')).hexdigest()
    return files, pins


class WorkflowConfigurationTests(unittest.TestCase):
    def setUp(self):
        self.runner = (WORKFLOWS / RUNNER).read_bytes()
        self.workflows = workflow_sources()

    def test_each_build_stops_on_makev_and_symlink_errors(self):
        for cc, script in build_scripts(self.workflows):
            for command in (f'.\\makev.bat -{cc}', '.\\v.exe symlink'):
                with self.subTest(compiler=cc, command=command, script=script):
                    self.assertIn(command + '\n' + EXIT_CHECK + '\n', script)

    def test_explicit_test_and_source_paths_exist(self):
        pattern = r'(?:vlib|thirdparty|cmd)[/\\][A-Za-z0-9_/\\.-]+\.(?:v|vsh|c)(?=[^A-Za-z0-9_.]|$)'
        for cc, source in self.workflows.items():
            paths = set(re.findall(pattern, source))
            self.assertTrue(paths, cc)
            for path in paths:
                with self.subTest(compiler=cc, path=path):
                    self.assertTrue((ROOT / path.replace('\\', '/')).is_file(), path)

    def test_current_w3_manifest_and_all_compiler_pins(self):
        surface, cases = validate_w3_metadata(self.runner, self.workflows)
        self.assertEqual(len(surface), 3)
        self.assertEqual(len(cases), 5)
        self.assertEqual({path for path, _ in surface}, {path for _, path, _ in cases})

    def test_stale_source_pin_is_rejected(self):
        surface, _ = validate_w3_metadata(self.runner, self.workflows)
        changed = self.runner.replace(surface[0][1].encode(), b'0' * 64)
        with self.assertRaisesRegex(ValueError, 'composite mismatch'):
            validate_w3_metadata(changed, self.workflows)

    def test_case_selection_drift_is_rejected(self):
        _, cases = validate_w3_metadata(self.runner, self.workflows)
        changed = self.runner.replace(cases[0][2].encode(), b'test_not_the_pinned_case')
        with self.assertRaisesRegex(ValueError, 'composite mismatch'):
            validate_w3_metadata(changed, self.workflows)

    def test_runner_edits_require_new_workflow_pins(self):
        with self.assertRaisesRegex(ValueError, 'pins do not match'):
            validate_w3_metadata(self.runner + b'\n', self.workflows)

    def test_each_stale_workflow_pin_is_rejected(self):
        digest = hashlib.sha256(self.runner).hexdigest()
        for cc in COMPILERS:
            with self.subTest(compiler=cc):
                changed = dict(self.workflows)
                changed[cc] = changed[cc].replace(digest, '0' * 64)
                with self.assertRaisesRegex(ValueError, 'pins do not match'):
                    validate_w3_metadata(self.runner, changed)

    def test_missing_w3_invocation_is_rejected(self):
        for cc in COMPILERS:
            with self.subTest(compiler=cc):
                changed = dict(self.workflows)
                changed[cc] = changed[cc].replace(RUNNER, 'missing.ps1')
                with self.assertRaisesRegex(ValueError, 'invocation or pins'):
                    validate_w3_metadata(self.runner, changed)

    def test_missing_build_step_is_rejected(self):
        changed = dict(self.workflows)
        changed['gcc'] = changed['gcc'].replace('.\\makev.bat -gcc', 'missing', 1)
        with self.assertRaisesRegex(ValueError, 'expected both Windows build steps'):
            build_scripts(changed)

    def test_file_hashes_are_byte_exact(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'input.v'
            content = b'fn test_probe() {}\n'
            path.write_bytes(content)
            expected = hashlib.sha256(content).hexdigest()
            validate_file_hash(path, expected)
            path.write_bytes(content.replace(b'\n', b'\r\n'))
            with self.assertRaisesRegex(ValueError, 'expected='):
                validate_file_hash(path, expected)
            path.unlink()
            with self.assertRaises(FileNotFoundError):
                validate_file_hash(path, expected)


class W3SourceTests(unittest.TestCase):
    def test_pinned_sources_and_cases_exist(self):
        surface, cases = validate_w3_metadata(
            (WORKFLOWS / RUNNER).read_bytes(), workflow_sources())
        for path, expected in surface:
            with self.subTest(path=path):
                validate_file_hash(ROOT / path, expected)
        for _, path, name in cases:
            with self.subTest(path=path, case=name):
                source = (ROOT / path).read_text(encoding='utf-8')
                self.assertRegex(source, r'(?m)^fn ' + re.escape(name) + r'\(\)')


class W5SourceTests(unittest.TestCase):
    def setUp(self):
        self.script = (WORKFLOWS / W5_RUNNER).read_text(encoding='utf-8')

    def test_sources_surfaces_and_all_compiler_callers(self):
        files, pins = w5_metadata(self.script)
        for path, expected in files.items():
            with self.subTest(path=path):
                validate_file_hash(ROOT / path, expected)
        for cc, workflow in workflow_sources().items():
            call = re.search(re.escape(W5_RUNNER) + r' -Compiler ' + cc + r' `(.*?)(?=^      -|\Z)',
                             workflow, re.M | re.S)
            self.assertIsNotNone(call, cc)
            supplied = dict(re.findall(r"-(Expected\w+) '([0-9a-f]{64})'", call[1]))
            self.assertEqual(supplied, pins, cc)
        workflow = (WORKFLOWS / 'makev_ci.yml').read_text(encoding='utf-8')
        for path in list(files) + [(WORKFLOWS / W5_RUNNER).relative_to(ROOT).as_posix()]:
            self.assertIn(f"      - '{path}'\n", workflow, path)

    def test_changed_source_pin_requires_a_new_composite(self):
        changed = re.sub(r"(Path = '[^']+'; Known = ')[0-9a-f]{64}",
                         r'\g<1>' + '0' * 64, self.script, count=1)
        with self.assertRaisesRegex(ValueError, 'composite mismatch'):
            w5_metadata(changed)

    def test_changed_test_selection_requires_a_new_tuple(self):
        with self.assertRaisesRegex(ValueError, 'tuple mismatch'):
            w5_metadata(self.script.replace("$caseName = 'test_", "$caseName = 'test_other_", 1))


class FallbackChecksumTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.shell = shutil.which('sh')
        compiler = shutil.which('gcc' if os.name == 'nt' else 'cc')
        if not cls.shell or not compiler:
            raise unittest.SkipTest('requires a POSIX shell and a C compiler')
        cls.temp = tempfile.TemporaryDirectory(prefix='fallback checksum stdin ')
        cls.addClassCleanup(cls.temp.cleanup)
        cls.root = Path(cls.temp.name)
        source = cls.root / 'sha256sum.c'
        source.write_text(r'''
#include <stdio.h>
#include <sys/stat.h>
int main(void) {
    struct stat info;
    /* Model Git for Windows coreutils checking stdin before hashing a file. */
    if (fstat(0, &info) != 0) {
        fputs("closed stdin\n", stderr);
        return 9;
    }
    puts("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef  fixture");
    return 0;
}
''', encoding='utf-8')
        binary = cls.root / ('sha256sum.exe' if os.name == 'nt' else 'sha256sum')
        subprocess.run([compiler, '-o', str(binary), str(source)],
                       check=True, capture_output=True, text=True, timeout=60)
        installer = (ROOT / 'cmd/tools/install_v1_fallback.sh').read_text(encoding='utf-8')
        start = installer.index('sha256_of() {\n')
        end = installer.index('\ncandidate_has_expected_version()', start)
        cls.function = installer[start:end]

    def checksum_with_closed_stdin(self, function):
        script = self.root / 'probe.sh'
        # Set PATH inside the shell: Git for Windows prepends its coreutils to
        # the inherited Windows PATH when sh starts. cygpath also converts the
        # drive prefix before POSIX PATH treats its colon as a separator.
        fixture_path = "'" + self.root.as_posix().replace("'", "'\\''") + "'"
        setup = (f'fixture_bin={fixture_path}\n'
                 'if command -v cygpath >/dev/null 2>&1; then\n'
                 '    fixture_bin=$(cygpath -u "$fixture_bin")\n'
                 'fi\nPATH="$fixture_bin:$PATH"\nexport PATH\n')
        script.write_bytes((setup + function + '\nsha256_of fixture 0<&-\n').encode('utf-8'))
        return subprocess.run([self.shell, script.as_posix()],
                              capture_output=True, text=True, timeout=30)

    def test_checksum_tools_receive_valid_stdin(self):
        result = self.checksum_with_closed_stdin(self.function)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout.strip(), '0123456789abcdef' * 4)

    def test_closed_stdin_fixture_detects_the_unredirected_regression(self):
        result = self.checksum_with_closed_stdin(self.function.replace('} < /dev/null', '}'))
        self.assertEqual(result.stdout.strip(), '')
        self.assertIn('closed stdin', result.stderr)


class ShaderFixtureTests(unittest.TestCase):
    def test_d3d11_fixtures_have_complete_dxbc_containers_without_sokol_gl(self):
        compiler = shutil.which('gcc' if os.name == 'nt' else 'cc')
        if not compiler:
            self.skipTest('requires a C compiler')
        with tempfile.TemporaryDirectory(prefix='multiwindow shader ') as folder:
            root = Path(folder)
            source = root / 'shader.c'
            source.write_text(r'''
#define SOKOL_D3D11
#include "shader_fixture_helpers.h"
#include <string.h>
static uint32_t read_u32(const uint8_t *data) {
    uint32_t value;
    memcpy(&value, data, sizeof(value));
    return value;
}
static int valid_dxbc(const void *blob, size_t size) {
    const uint8_t *data = blob;
    if (size < 32 || memcmp(data, "DXBC", 4) || read_u32(data + 24) != size) return 0;
    uint32_t count = read_u32(data + 28);
    if (!count || count > (size - 32) / 4) return 0;
    for (uint32_t i = 0; i < count; i++) {
        uint32_t offset = read_u32(data + 32 + i * 4);
        if (offset > size - 8 || read_u32(data + offset + 4) > size - offset - 8) return 0;
    }
    return 1;
}
int main(void) {
    return !valid_dxbc(v_multiwindow_fixture_hlsl4_vs(), v_multiwindow_fixture_hlsl4_vs_size())
        || !valid_dxbc(v_multiwindow_fixture_hlsl4_fs(), v_multiwindow_fixture_hlsl4_fs_size());
}
''', encoding='utf-8')
            binary = root / ('shader.exe' if os.name == 'nt' else 'shader')
            subprocess.run([compiler, '-std=c11', '-Werror', '-I', str(ROOT / 'vlib/gg/testdata/'
                            'multiwindow_sokol_trace'), '-o', str(binary), str(source)],
                           check=True, capture_output=True, text=True, timeout=60)
            result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


class ToolCacheVerificationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.pwsh = shutil.which('pwsh')
        compiler = shutil.which('gcc' if os.name == 'nt' else 'cc')
        if not cls.pwsh or not compiler:
            raise unittest.SkipTest('requires PowerShell and a C compiler')
        cls.temp = tempfile.TemporaryDirectory(prefix='windows tool cache probe ')
        cls.addClassCleanup(cls.temp.cleanup)
        cls.root = Path(cls.temp.name)
        source = cls.root / 'probe.c'
        source.write_text(r'''
#include <stdio.h>
#include <stdlib.h>
int main(void) {
    FILE *calls = fopen(getenv("VTEST_CI_CALLS"), "a");
    if (!calls) return 90;
    fprintf(calls, "%s\n", getenv("VTOOLS_CACHE_VERBOSE"));
    fclose(calls);
    puts(getenv("VTEST_TOOL_CACHE_MESSAGE"));
    return atoi(getenv("VTEST_TOOL_CACHE_EXIT"));
}
''', encoding='utf-8')
        subprocess.run([compiler, '-o', str(cls.root / 'v.exe'), str(source)],
                       check=True, capture_output=True, text=True, timeout=60)

    def run_probe(self, message, status=0):
        calls = self.root / 'calls.txt'
        calls.unlink(missing_ok=True)
        env = dict(os.environ, VTEST_CI_CALLS=str(calls),
                   VTEST_TOOL_CACHE_MESSAGE=message, VTEST_TOOL_CACHE_EXIT=str(status),
                   VTOOLS_CACHE_VERBOSE='original')
        # Invoke from a wrapper to check that the script returns to its caller
        # and restores the environment before test-cleancode starts.
        script = self.root / 'verify.ps1'
        path = str(WORKFLOWS / 'setup_windows_tool_cache.ps1').replace("'", "''")
        script.write_text(
            f"& '{path}' -Verify\n"
            "if ($env:VTOOLS_CACHE_VERBOSE -ne 'original') { throw 'environment changed' }\n"
            "Write-Host 'returned to caller'\n", encoding='utf-8')
        result = subprocess.run([self.pwsh, '-NoProfile', '-NonInteractive', '-File', str(script)],
                                cwd=self.root, env=env, capture_output=True, text=True, timeout=30)
        return result, calls.read_text().splitlines()

    def test_cached_tool_is_reused_and_returns_to_the_workflow(self):
        result, calls = self.run_probe('> reusing the cached `vvet` at probe')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('returned to caller', result.stdout)
        self.assertEqual(calls, ['1', '1'])

    def test_recompiling_or_uncached_tools_are_rejected(self):
        for message in ('> recompiling `vvet`', '> running `vvet` from source', ''):
            with self.subTest(message=message):
                result, calls = self.run_probe(message)
                self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(calls, ['1', '1'])

    def test_a_failed_probe_stops_before_the_second_invocation(self):
        result, calls = self.run_probe('controlled vet failure', status=47)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('exit code 47', result.stderr)
        self.assertEqual(calls, ['1'])


@unittest.skipUnless(os.name == 'nt', 'requires Windows process execution')
class WindowsBuildExecutionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.gcc = shutil.which('gcc')
        cls.pwsh = shutil.which('pwsh')
        if not cls.gcc or not cls.pwsh:
            raise RuntimeError('the Windows execution tests require GCC and PowerShell')
        cls.temp = tempfile.TemporaryDirectory(prefix='windows ci contracts ')
        cls.addClassCleanup(cls.temp.cleanup)
        cls.root = Path(cls.temp.name)
        source = cls.root / 'probe.c'
        source.write_text(r'''
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(int argc, char **argv) {
    FILE *calls = fopen(getenv("VTEST_CI_CALLS"), "a");
    if (!calls) return 90;
    for (int i = 1; i < argc; ++i) fprintf(calls, "%s%s", i == 1 ? "" : " ", argv[i]);
    fputc('\n', calls);
    fclose(calls);
    const char *key = argc > 1 && strcmp(argv[1], "symlink") == 0
        ? "VTEST_SYMLINK_EXIT" : "VTEST_STATS_EXIT";
    const char *value = getenv(key);
    return value ? atoi(value) : 0;
}
''', encoding='utf-8')
        subprocess.run([cls.gcc, '-std=c11', '-Wall', '-Wextra', '-Werror',
                        '-o', str(cls.root / 'v.exe'), str(source)],
                       check=True, capture_output=True, text=True, timeout=60)
        (cls.root / 'makev.bat').write_bytes(
            b'@echo off\r\nexit /b %VTEST_MAKEV_EXIT%\r\n')

    def run_build(self, script, makev=0, symlink=0, stats=0):
        calls = self.root / 'calls.txt'
        calls.unlink(missing_ok=True)
        env = dict(os.environ, VTEST_CI_CALLS=str(calls), VTEST_MAKEV_EXIT=str(makev),
                   VTEST_SYMLINK_EXIT=str(symlink), VTEST_STATS_EXIT=str(stats))
        # Use the same prefix/suffix as GitHub Actions' built-in pwsh shell.
        body = "$ErrorActionPreference = 'Stop'\n" + script + (
            '\nif (Test-Path -LiteralPath variable:\\LASTEXITCODE) { exit $LASTEXITCODE }\n')
        path = self.root / 'build.ps1'
        path.write_text(body, encoding='utf-8')
        result = subprocess.run([self.pwsh, '-NoProfile', '-NonInteractive', '-File', str(path)],
                                cwd=self.root, env=env, capture_output=True, text=True, timeout=30)
        observed = calls.read_text(encoding='utf-8').splitlines() if calls.exists() else []
        return result, observed

    def test_makev_failure_stops_before_a_working_v(self):
        for cc, script in build_scripts(workflow_sources()):
            with self.subTest(compiler=cc, script=script):
                result, calls = self.run_build(script, makev=47)
                self.assertEqual(result.returncode, 47, result.stdout + result.stderr)
                self.assertEqual(calls, [])

    def test_symlink_failure_stops_before_later_commands(self):
        for cc, script in build_scripts(workflow_sources()):
            with self.subTest(compiler=cc, script=script):
                result, calls = self.run_build(script, symlink=48)
                self.assertEqual(result.returncode, 48, result.stdout + result.stderr)
                self.assertEqual(calls, ['symlink'])

    def test_success_keeps_all_follow_up_commands(self):
        for cc, script in build_scripts(workflow_sources()):
            with self.subTest(compiler=cc, script=script):
                result, calls = self.run_build(script)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                expected = re.findall(r'^\.\\v\.exe (.+)$', script, re.M)
                self.assertEqual(calls, expected)

    def test_final_ecdsa_failure_is_preserved(self):
        scripts = [s for _, s in build_scripts(workflow_sources()) if '-stats' in s]
        self.assertEqual(len(scripts), 1)
        result, calls = self.run_build(scripts[0], stats=49)
        self.assertEqual(result.returncode, 49, result.stdout + result.stderr)
        self.assertEqual(len(calls), 2)

    def test_prepared_cache_passes_the_compilers_native_privacy_check(self):
        source = self.root / 'cache_privacy.c'
        source.write_text(r'''
#define V_TOOLCACHE_WINDOWS_SCOPE
#include "toolcache_windows_helpers.h"
int wmain(int argc, wchar_t **argv) {
    return argc != 2 || !v_toolcache_root_is_private((unsigned short *)argv[1]);
}
''', encoding='utf-8')
        checker = self.root / 'cache_privacy.exe'
        subprocess.run([self.gcc, '-municode', '-I', str(ROOT / 'cmd' / 'v'),
                        '-o', str(checker), str(source), '-ladvapi32'],
                       check=True, capture_output=True, text=True, timeout=60)
        github_env = self.root / 'github_env.txt'
        github_env.unlink(missing_ok=True)
        env = dict(os.environ, RUNNER_TEMP=str(self.root), GITHUB_ENV=str(github_env))
        prepared = subprocess.run(
            [self.pwsh, '-NoProfile', '-NonInteractive', '-File',
             str(WORKFLOWS / 'setup_windows_tool_cache.ps1'), '-CacheParent', str(self.root)],
            env=env, capture_output=True, text=True, timeout=30)
        self.assertEqual(prepared.returncode, 0, prepared.stdout + prepared.stderr)
        assignments = dict(line.split('=', 1) for line in
                           github_env.read_text(encoding='utf-8-sig').splitlines())
        cache = Path(assignments['VTOOLS_CACHE_DIR'])
        self.assertEqual(cache.parent, self.root)
        temporary = Path(assignments['VTMP'])
        self.assertEqual(temporary.parent, cache)
        self.assertTrue(temporary.is_dir())
        checked = subprocess.run([str(checker), str(cache)], capture_output=True, timeout=30)
        self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=2)
