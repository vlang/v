"""Check the Win32 W4 integrity chain without building V or starting GUI tests.

Run from a complete checkout: python cmd/tools/windows_w4_ci_test.py
Hashes use exact file bytes; no whitespace normalization or automatic repinning.
"""

import hashlib
import re
import unittest
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parents[2]
W4 = '.github/workflows/run_multiwindow_win32_w4.ps1'
W3 = '.github/workflows/run_multiwindow_win32_w3_green.ps1'
WORKFLOW = '.github/workflows/makev_ci.yml'
COMPILERS = ('gcc', 'msvc', 'tcc')
TEST_PARAMETERS = ('Oracle', 'NativeTest', 'PublicTest', 'NoOptProbe')
PRODUCTION_PARAMETERS = (
    'ServiceBackend', 'EventDelivery', 'Win32Backend',
    'Win32ServiceBackend', 'Win32ServiceNative',
)


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def one(pattern: str, text: str) -> str:
    matches = re.findall(pattern, text, re.MULTILINE | re.DOTALL)
    require(len(matches) == 1, f'expected one match for {pattern!r}, got {len(matches)}')
    return matches[0]


def literals(body: str) -> list[str]:
    require(re.fullmatch(r"(?:\s*'[^'\n]*')*\s*", body) is not None,
            'expected a literal-only ordered array')
    return re.findall(r"'([^'\n]*)'", body)


def array(text: str, name: str) -> list[str]:
    return literals(one(r'^\$' + re.escape(name) + r' = @\((.*?)^\)', text))


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def records_hash(records: list[str]) -> str:
    return sha256(('\n'.join(records) + '\n').encode('utf-8'))


@dataclass
class Surface:
    paths: list[str]
    hashes: list[str]
    cases: list[tuple[str, str, str, str]]
    production_paths: list[str]
    production_hashes: dict[str, list[str]]
    declared_tuple: str

    def tuple_hash(self) -> str:
        return records_hash(
            ['schema=package2-win32-w4-test-tuple-v1']
            + [f'file={path}|sha256={digest}' for path, digest in zip(self.paths, self.hashes)]
            + [f'case={kind}|{name}|{family}' for kind, _, name, family in self.cases]
        )

    def production_hash(self, state: str) -> str:
        return records_hash(
            ['schema=package2-win32-w4-production-surface-v1', f'expectation={state}']
            + [f'file={path}|sha256={digest}' for path, digest
               in zip(self.production_paths, self.production_hashes[state])]
        )


def parse_surface(text: str) -> Surface:
    variables = re.findall(r'\$(\w+)', one(r'^\$testPaths = @\((.*?)\)$', text))
    require(variables == ['oracle', 'nativeTest', 'publicTest', 'noOptProbe'],
            'W4 test-path order changed')
    paths = [one(r'^\$' + name + r" = '([^']+)'$", text) for name in variables]
    path_by_variable = dict(zip(variables, paths))
    case_body = one(r'^\$cases = @\((.*?)^\)', text)
    pattern = (r"\[pscustomobject\]@\{\s*Kind = '([^']+)'\s*File = \$(\w+)"
               r"\s*Name = '(test_[A-Za-z0-9_]+)'\s*Family = '([^']+)'\s*\}")
    raw_cases = re.findall(pattern, case_body)
    require(re.sub(pattern, '', case_body).strip() == '', 'unparsed W4 case')
    require(len(raw_cases) == 8, 'W4 must retain eight cases')
    require(len({case[2] for case in raw_cases}) == 8, 'duplicate W4 test name')
    require(len({case[3] for case in raw_cases}) == 8, 'duplicate W4 family')
    require(all(case[1] in path_by_variable for case in raw_cases), 'unknown W4 case file')
    cases = [(kind, path_by_variable[var], name, family)
             for kind, var, name, family in raw_cases]
    require([case[0] for case in cases] == ['native'] * 7 + ['public'],
            'W4 native/public case coverage changed')
    require(all(path == path_by_variable['nativeTest' if kind == 'native' else 'publicTest']
                for kind, path, _, _ in cases), 'W4 case routed to the wrong file')
    production_body = one(r'^\$knownSurfaceFiles = @\{(.*?)^\}', text)
    production_hashes = {
        state: literals(one(r'^    ' + state + r' = @\((.*?)^    \)', production_body))
        for state in ('Red', 'Green')
    }
    surface = Surface(
        paths, array(text, 'knownTestFileHashes'), cases, array(text, 'surfacePaths'),
        production_hashes, one(r"^\$knownTestTupleSha256 = '([0-9a-f]{64})'$", text),
    )
    require(len(surface.hashes) == 4, 'W4 must pin four test inputs')
    require(len(surface.production_paths) == 5, 'W4 must pin five production inputs')
    require(all(len(values) == 5 for values in production_hashes.values()),
            'W4 production hash count changed')
    for digest in surface.hashes + sum(production_hashes.values(), []):
        require(re.fullmatch('[0-9a-f]{64}', digest) is not None, 'invalid W4 SHA-256')
    require(surface.tuple_hash() == surface.declared_tuple, 'W4 ordered test tuple mismatch')
    return surface


def validate_shared_pins(surface: Surface, w3_text: str) -> None:
    entries = dict(re.findall(r"Path = '([^']+)'\s+Hash = '([0-9a-f]{64})'", w3_text))
    for index in (1, 2):
        path = surface.paths[index]
        require(entries.get(path) == surface.hashes[index], f'W3/W4 pin mismatch for {path}')


def validate_caller(script: bytes, workflow: str, compiler: str) -> None:
    surface = parse_surface(script.decode('utf-8'))
    step = one(r'^      - name: Win32 W4 clipboard contracts\n(.*?)(?=^      - |\Z)', workflow)
    require('        shell: pwsh\n' in step, 'W4 requires PowerShell')
    require("          VFLAGS: ''\n" in step, 'W4 must isolate VFLAGS')
    require(step.count('run_multiwindow_win32_w4.ps1') == 1, 'missing/duplicate W4 invocation')
    require(re.findall(r'-Compiler (\w+)', step) == [compiler], 'wrong W4 compiler')
    require(re.findall(r'-Expectation (\w+)', step) == ['Green'], 'W4 must expect Green')
    pairs = re.findall(r"-(Expected\w+Sha256) '([0-9a-fA-F]{64})'", step)
    supplied = {key: value.lower() for key, value in pairs}
    require(len(supplied) == len(pairs), 'duplicate W4 hash parameter')
    expected = {f'Expected{name}Sha256': digest
                for name, digest in zip(TEST_PARAMETERS, surface.hashes)}
    expected.update({f'Expected{name}Sha256': digest for name, digest
                     in zip(PRODUCTION_PARAMETERS, surface.production_hashes['Green'])})
    for state in ('Red', 'Green'):
        expected[f'Expected{state}SurfaceSha256'] = surface.production_hash(state)
    expected['ExpectedRunnerSha256'] = sha256(script)
    require(supplied == expected, f'W4 caller pins differ for {compiler}: '
            f'{[(key, supplied.get(key), value) for key, value in expected.items() if supplied.get(key) != value]}')


def validate_sources(surface: Surface, read: Callable[[str], bytes]) -> None:
    expected = dict(zip(surface.paths, surface.hashes))
    expected.update(zip(surface.production_paths, surface.production_hashes['Green']))
    for path, digest in expected.items():
        actual = sha256(read(path))
        require(actual == digest, f'W4 source hash mismatch for {path}: expected={digest} actual={actual}')
    for _, path, name, _ in surface.cases:
        matches = re.findall(r'^fn ' + re.escape(name) + r'\(\)', read(path).decode('utf-8'), re.MULTILINE)
        require(len(matches) == 1, f'W4 test must be discovered exactly once: {name} in {path}')


class W4Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.script = (ROOT / W4).read_bytes()
        cls.text = cls.script.decode('utf-8')
        cls.w3_text = (ROOT / W3).read_text(encoding='utf-8')
        cls.callers = {cc: (ROOT / f'.github/workflows/windows_ci_{cc}.yml').read_text(encoding='utf-8')
                       for cc in COMPILERS}

    def test_configuration_ordered_manifests(self) -> None:
        parse_surface(self.text)

    def test_configuration_shared_w3_w4_pins(self) -> None:
        validate_shared_pins(parse_surface(self.text), self.w3_text)

    def test_configuration_all_compiler_callers(self) -> None:
        for cc, workflow in self.callers.items():
            with self.subTest(compiler=cc):
                validate_caller(self.script, workflow, cc)

    def test_configuration_fast_workflow_covers_all_inputs(self) -> None:
        workflow = (ROOT / WORKFLOW).read_text(encoding='utf-8')
        surface = parse_surface(self.text)
        paths = surface.paths + surface.production_paths + [W4, W3, 'cmd/tools/windows_w4_ci_test.py']
        for event in ('push', 'pull_request'):
            body = one(r'^  ' + event + r':\n(.*?)(?=^  \w|\Z)', workflow)
            for path in paths:
                self.assertIn(f"      - '{path}'\n", body, (event, path))
        self.assertIn('run: python cmd/tools/windows_w4_ci_test.py', workflow)
        self.assertIn('run: python cmd/tools/windows_ci_test.py', workflow)
        self.assertIn('run: python cmd/tools/makev_test.py', workflow)

    def test_checked_in_source_bytes_and_case_discovery(self) -> None:
        # Deliberately fail for an incomplete checkout: CI must not silently skip integrity checks.
        validate_sources(parse_surface(self.text), lambda path: (ROOT / path).read_bytes())

    def test_negative_runner_bytes(self) -> None:
        for changed in (self.script + b'\n', self.script.replace(b'\n', b'\r\n')):
            for cc, workflow in self.callers.items():
                with self.subTest(compiler=cc), self.assertRaises(AssertionError):
                    validate_caller(changed, workflow, cc)

    def test_negative_caller_hashes(self) -> None:
        for cc, workflow in self.callers.items():
            step = one(r'^      - name: Win32 W4 clipboard contracts\n(.*?)(?=^      - |\Z)', workflow)
            for name, digest in re.findall(r"-(Expected\w+Sha256) '([0-9a-f]{64})'", step):
                changed_step = step.replace(f"-{name} '{digest}'", f"-{name} '{'0' * 64}'")
                changed = workflow.replace(step, changed_step)
                with self.subTest(compiler=cc, parameter=name), self.assertRaises(AssertionError):
                    validate_caller(self.script, changed, cc)

    def test_negative_missing_duplicate_or_wrong_caller(self) -> None:
        for cc, workflow in self.callers.items():
            label = '      - name: Win32 W4 clipboard contracts\n'
            mutations = [workflow.replace(label, label.replace('W4', 'W9')),
                         workflow + '\n' + workflow,
                         workflow.replace(f'-Compiler {cc} `', '-Compiler invalid `'),
                         workflow.replace('-Expectation Green `', '-Expectation Red `')]
            for changed in mutations:
                with self.subTest(compiler=cc), self.assertRaises(AssertionError):
                    validate_caller(self.script, changed, cc)

    def test_negative_case_manifest_drift(self) -> None:
        surface = parse_surface(self.text)
        changed = self.text.replace(surface.cases[0][2], 'test_unreviewed_clipboard_case')
        with self.assertRaisesRegex(AssertionError, 'ordered test tuple'):
            parse_surface(changed)
        with self.assertRaises(AssertionError):
            parse_surface(self.text.replace("Kind = 'public'", "Kind = 'native'", 1))

    def test_negative_shared_pin_drift(self) -> None:
        surface = parse_surface(self.text)
        for digest in surface.hashes[1:3]:
            with self.assertRaisesRegex(AssertionError, 'W3/W4 pin mismatch'):
                validate_shared_pins(surface, self.w3_text.replace(digest, '0' * 64))

    def source_fixture(self) -> tuple[Surface, dict[str, bytes]]:
        surface = parse_surface(self.text)
        files = {path: b'// fixture\n' for path in surface.paths + surface.production_paths}
        for _, path, name, _ in surface.cases:
            files[path] += f'fn {name}() {{ assert true }}\n'.encode('utf-8')
        return replace(surface, hashes=[sha256(files[path]) for path in surface.paths],
                       production_hashes={'Green': [sha256(files[path]) for path in surface.production_paths]}), files

    def test_negative_source_byte_and_line_ending_changes(self) -> None:
        surface, files = self.source_fixture()
        validate_sources(surface, files.__getitem__)
        for path, content in files.items():
            for changed in (content + b' ', content.replace(b'\n', b'\r\n')):
                mutated = dict(files, **{path: changed})
                with self.subTest(path=path), self.assertRaisesRegex(AssertionError, 'source hash mismatch'):
                    validate_sources(surface, mutated.__getitem__)

    def test_negative_missing_source(self) -> None:
        surface, files = self.source_fixture()
        for path in files:
            missing = files.copy()
            del missing[path]
            with self.subTest(path=path), self.assertRaises(KeyError):
                validate_sources(surface, missing.__getitem__)

    def test_negative_missing_or_duplicate_case_even_with_updated_hash(self) -> None:
        surface, files = self.source_fixture()
        _, path, name, _ = surface.cases[0]
        for changed in (files[path].replace(name.encode(), b'test_missing_case'),
                        files[path] + f'fn {name}() {{}}\n'.encode()):
            mutated = dict(files, **{path: changed})
            updated = replace(surface, hashes=[sha256(mutated[p]) for p in surface.paths])
            with self.assertRaisesRegex(AssertionError, 'discovered exactly once'):
                validate_sources(updated, mutated.__getitem__)


if __name__ == '__main__':
    unittest.main()
