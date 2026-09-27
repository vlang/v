"""Execute the BSD Tools CI shell steps with controlled command failures.

Run with `python3 cmd/tools/tools_ci_test.py` on a POSIX host. No V compiler,
BSD VM, package installation, third-party Python module, or network is needed.
The workflow's literal shell blocks are executed unchanged; only external
commands and ulimit are replaced with fixtures in an isolated temporary tree.
"""

from __future__ import annotations

import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "tools_ci.yml"
COMPILERS = {
    "tools-freebsd": ("tcc", "gcc", "clang"),
    "tools-openbsd": ("tcc", "clang"),
}


def job_text(workflow: str, job: str) -> str:
    """Read one top-level job, rejecting missing or ambiguous matches."""
    matches = re.findall(
        rf"(?ms)^  {re.escape(job)}:\n(.*?)(?=^  [\w-]+:\n|\Z)", workflow
    )
    if len(matches) != 1:
        raise AssertionError(f"expected exactly one {job} job, found {len(matches)}")
    return matches[0]


def bsd_script(job: str) -> str:
    """Extract the known cpa.sh literal block, not arbitrary YAML syntax.

    Fail when the workflow structure changes rather than silently testing a
    different step. Keeping this small reader strict avoids a PyYAML dependency
    for a compiler-independent CI check.
    """
    matches = re.findall(
        r"(?m)^        shell: cpa\.sh \{0\}\n"
        r"        run: \|\n((?:          [^\n]*\n|\n)+)",
        job,
    )
    if len(matches) != 1:
        raise AssertionError(f"expected exactly one cpa.sh literal block, found {len(matches)}")
    script = "".join(line[10:] if line.startswith("          ") else line
                     for line in matches[0].splitlines(keepends=True))
    if "${{" in script:
        raise AssertionError("substitute Actions expressions before testing this shell block")
    # YAML | uses clip chomping: preserve exactly one final newline.
    return script.rstrip("\n") + "\n"


def expected_commands(job: str, compiler: str) -> list[str]:
    """The existing workload must still execute, in order, on successful runs."""
    if job == "tools-freebsd":
        commands = ["pkg install -y git sqlite3 gmake boehm-gc-threaded libiconv"]
        if compiler == "gcc":
            commands.append("pkg install -y gcc")
        commands.append("hostname -s freebsd-ci")
    else:
        commands = [
            "pkg_add git sqlite3 gmake boehm-gc libiconv openssl",
            "hostname -s openbsd-ci",
        ]
    commands += ["uname -a", "git config --global --add safe.directory ."]
    if job == "tools-openbsd":
        commands.append("ulimit -d 4194304")
    commands += [
        "gmake",
        "v -showcc -o v cmd/v",
        "v symlink",
        "v doctor",
        "v fmt -verify cmd/",
        "v -silent -N -W -check build-tools",
        "v -silent test-self cmd",
    ]
    if compiler != "tcc":
        commands.append("v -silent -W -cstrict test-self cmd")
    return commands


class ToolsCiShellTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.workflow = WORKFLOW.read_text(encoding="utf-8")
        cls.scripts = {job: bsd_script(job_text(cls.workflow, job)) for job in COMPILERS}
        sh = shutil.which("sh")
        if sh is None or os.name == "nt":
            raise AssertionError("these shell contracts require a POSIX host with sh")
        cls.sh = os.path.abspath(sh)
        cls.shells = [cls.sh]
        bash = shutil.which("bash")
        if bash is not None:
            cls.shells.append(os.path.abspath(bash))

    def execute(
        self, script: str, shell: str, errexit: bool, compiler: str,
        failure: str = "", code: int = 0,
    ) -> tuple[subprocess.CompletedProcess[str], list[str]]:
        with tempfile.TemporaryDirectory(prefix="v tools ci ") as directory:
            root = Path(directory)
            bin_dir = root / "bin"
            bin_dir.mkdir()
            log = root / "commands.log"
            probe = root / "probe"
            shebang = f"#!{self.sh}\n"
            probe.write_text(shebang + '''label=$1
shift
for arg in "$@"; do
    label="$label $arg"
done
printf '%s\\n' "$label" >> "$CI_TEST_LOG"
if [ "$label" = "$CI_TEST_FAIL" ]; then
    exit "$CI_TEST_CODE"
fi
exit 0
''', encoding="utf-8")
            probe.chmod(0o755)
            for command in ("pkg", "pkg_add", "hostname", "uname", "git", "gmake", "v"):
                path = root / command if command == "v" else bin_dir / command
                path.write_text(
                    shebang + f'exec "$CI_TEST_PROBE" {command} "$@"\n', encoding="utf-8"
                )
                path.chmod(0o755)
            sudo = bin_dir / "sudo"
            sudo.write_text(shebang + 'exec "$@"\n', encoding="utf-8")
            sudo.chmod(0o755)
            # PATH contains only fixtures: no test can install packages, change
            # the host hostname, modify git configuration, or run a real V build.
            environment = {
                "PATH": str(bin_dir),
                "LC_ALL": "C",
                "VFLAGS": f"-cc {compiler}",
                "CI_TEST_PROBE": str(probe),
                "CI_TEST_LOG": str(log),
                "CI_TEST_FAIL": failure,
                "CI_TEST_CODE": str(code),
            }
            # ulimit is a shell builtin; do not change the host's real limits.
            prelude = 'ulimit() { "$CI_TEST_PROBE" ulimit "$@"; }\n'
            args = [shell] + (["-e"] if errexit else []) + ["-s"]
            result = subprocess.run(
                args, input=prelude + script, cwd=root, env=environment,
                capture_output=True, text=True, timeout=5, check=False,
            )
            commands = log.read_text(encoding="utf-8").splitlines() if log.exists() else []
            return result, commands

    def test_existing_bsd_compiler_matrices_are_covered(self) -> None:
        for job, compilers in COMPILERS.items():
            with self.subTest(job=job):
                matches = re.findall(
                    r"(?m)^        cc: \[([^\]]+)\]$", job_text(self.workflow, job)
                )
                self.assertEqual(len(matches), 1)
                actual = tuple(value.strip() for value in matches[0].split(","))
                self.assertEqual(actual, compilers)

    def test_success_keeps_every_existing_command(self) -> None:
        for job, compilers in COMPILERS.items():
            for compiler in compilers:
                for shell in self.shells:
                    for errexit in (False, True):
                        with self.subTest(
                            job=job, compiler=compiler, shell=shell, errexit=errexit
                        ):
                            result, trace = self.execute(
                                self.scripts[job], shell, errexit, compiler
                            )
                            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                            self.assertEqual(trace, expected_commands(job, compiler))

    def test_failure_preserves_exit_code_and_stops_followups(self) -> None:
        for job, compilers in COMPILERS.items():
            for compiler in compilers:
                expected = expected_commands(job, compiler)
                for shell in self.shells:
                    for errexit in (False, True):
                        for index, failure in enumerate(expected):
                            with self.subTest(job=job, compiler=compiler, shell=shell,
                                              errexit=errexit, failure=failure):
                                # Non-1 codes prove that the originating status
                                # survives, rather than being replaced by a generic failure.
                                code = 41 + index
                                result, trace = self.execute(
                                    self.scripts[job], shell, errexit, compiler, failure, code
                                )
                                self.assertEqual(result.returncode, code,
                                                 f"{trace}\n{result.stdout}\n{result.stderr}")
                                self.assertEqual(trace, expected[:index + 1])


if __name__ == "__main__":
    unittest.main(verbosity=2)
