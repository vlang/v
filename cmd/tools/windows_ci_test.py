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


class WorkflowConfigurationTests(unittest.TestCase):
    def setUp(self):
        self.runner = (WORKFLOWS / RUNNER).read_bytes()
        self.workflows = workflow_sources()

    def test_each_build_stops_on_makev_and_symlink_errors(self):
        for cc, script in build_scripts(self.workflows):
            for command in (f'.\\makev.bat -{cc}', '.\\v.exe symlink'):
                with self.subTest(compiler=cc, command=command, script=script):
                    self.assertIn(command + '\n' + EXIT_CHECK + '\n', script)

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


if __name__ == '__main__':
    unittest.main(verbosity=2)
