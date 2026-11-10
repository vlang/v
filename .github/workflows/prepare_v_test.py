"""Exercise compiler setup without downloading or compiling a real toolchain."""

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
SHA = "0123456789abcdef0123456789abcdef01234567"
ISOLATED = ("VFLAGS", "VEXE", "VPROD", "MAKEFLAGS", "MFLAGS")

MOCK = r'''
import json
import os
from pathlib import Path
import shutil
import sys

tool = Path(sys.argv[0]).name
args = sys.argv[1:]
with open(os.environ["MOCK_LOG"], "a") as log:
    log.write(json.dumps({"tool": tool, "args": args, "cwd": os.getcwd(),
        "env": {key: os.environ.get(key) for key in
                ("VFLAGS", "VEXE", "VPROD", "MAKEFLAGS", "MFLAGS")}}) + "\n")
if tool == "git":
    assert args == ["rev-parse", "HEAD^{commit}"], args
    print(os.environ["MOCK_SHA"])
elif tool == "uname":
    print(os.environ["MOCK_ARCH" if args == ["-m"] else "MOCK_OS"])
elif tool == "getconf":
    print(os.environ["MOCK_LIBC"])
elif tool == "sw_vers":
    print(os.environ.get("MOCK_MACOS", "15.0"))
elif tool == "curl":
    source = os.environ.get("MOCK_RELEASE")
    if not source:
        sys.exit(22)
    url = next(arg for arg in args if arg.startswith("https://"))
    if url.endswith(".sha256"):
        source += ".sha256"
    shutil.copyfile(source, args[args.index("-o") + 1])
elif tool == "make":
    if "-C" in args:
        project = Path(args[args.index("-C") + 1])
        assert (project / "generated").is_file(), project
        if os.environ.get("MOCK_FAIL_BUILD"):
            (project / "v").write_text("incomplete production compiler")
            sys.exit(23)
        shutil.copyfile(os.environ["MOCK_PRODUCTION"], project / "v")
    elif not any(arg in args for arg in
                 ("latest_vc", "latest_tcc", "latest_legacy", "cache-basic-modules")):
        shutil.copyfile(os.environ["MOCK_BOOTSTRAP"], "v")
        Path("v").chmod(0o755)
'''

COMPILER = r'''
import json
import os
from pathlib import Path
import sys

args = sys.argv[1:]
with open(os.environ["MOCK_LOG"], "a") as log:
    log.write(json.dumps({"tool": "compiler", "identity": COMPILER_ID,
        "args": args, "env": {key: os.environ.get(key) for key in
        ("VFLAGS", "VEXE", "VPROD", "MAKEFLAGS", "MFLAGS")}}) + "\n")
if "-generate-c-project" in args:
    assert args[0] == "self" and "-prod" in args, args
    Path(args[args.index("-generate-c-project") + 1], "generated").touch()
elif args == ["-version"]:
    if os.environ.get("MOCK_FAIL_VERSION"):
        sys.exit(24)
    print("V optimized compiler " + os.environ["MOCK_SHA"])
'''


class PrepareVTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="prepare-v-test-")
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name)
        self.repo = self.base / "你好 my $path, @с интервали"
        workflows = self.repo / ".github/workflows"
        workflows.mkdir(parents=True)
        shutil.copyfile(ROOT / ".github/workflows/prepare_v.sh", workflows / "prepare_v.sh")
        build_tools = self.repo / "cmd/tools"
        build_tools.mkdir(parents=True)
        shutil.copyfile(ROOT / "cmd/tools/build_vprod.sh", build_tools / "build_vprod.sh")
        self.bin = self.base / "bin"
        self.bin.mkdir()
        for tool in ("make", "curl", "git", "uname", "getconf", "sw_vers", "clang", "gcc"):
            self.executable(self.bin / tool, MOCK)
        self.bootstrap = self.base / "bootstrap"
        self.production = self.base / "production"
        self.executable(self.bootstrap, COMPILER.replace("COMPILER_ID", repr("bootstrap")))
        self.executable(self.production, COMPILER.replace("COMPILER_ID", repr("production")))
        shutil.copyfile(self.bootstrap, self.repo / "v")
        (self.repo / "v").chmod(0o755)
        self.original = (self.repo / "v").read_bytes()
        self.log = self.base / "commands.jsonl"
        temporary_files = self.base / "runner temp"
        temporary_files.mkdir()
        self.env = dict(os.environ)
        for key in list(self.env):
            if key.startswith(("V_CI_", "MOCK_")) or key in ISOLATED:
                self.env.pop(key)
        self.env.update({
            "PATH": str(self.bin) + os.pathsep + os.environ["PATH"],
            "RUNNER_TEMP": str(temporary_files),
            "TMPDIR": str(temporary_files),
            "MOCK_LOG": str(self.log),
            "MOCK_SHA": SHA,
            "MOCK_OS": "Linux",
            "MOCK_ARCH": "x86_64",
            "MOCK_LIBC": "glibc 2.35",
            "MOCK_BOOTSTRAP": str(self.bootstrap),
            "MOCK_PRODUCTION": str(self.production),
            "VFLAGS": "-cc tcc -d test_configuration",
            "VEXE": "/must-not-build-with-this-compiler",
            "VPROD": "/must-not-recursively-build-this",
            "MAKEFLAGS": "inherited-make-flags",
            "MFLAGS": "inherited-short-make-flags",
        })

    def executable(self, path, source):
        path.write_text("#!" + sys.executable + "\n" + source)
        path.chmod(0o755)

    def run_prepare(self, *args, **env):
        return subprocess.run(
            ["sh", ".github/workflows/prepare_v.sh", *args],
            cwd=self.repo, env={**self.env, **env}, capture_output=True, text=True,
            timeout=15,
        )

    def events(self, tool=None):
        events = [json.loads(line) for line in self.log.read_text().splitlines()]
        return [event for event in events if tool is None or event["tool"] == tool]

    def asset(self, name=None, digest=None):
        folder = self.base / "downloaded artifacts"
        folder.mkdir(exist_ok=True)
        path = folder / (name or f"{SHA}-linux-x64")
        shutil.copyfile(self.production, path)
        checksum = digest if digest is not None else hashlib.sha256(path.read_bytes()).hexdigest()
        Path(str(path) + ".sha256").write_text(checksum + "  " + path.name + "\n")
        return path

    def assert_success(self, result):
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual((self.repo / "v").read_bytes(), self.production.read_bytes())

    def assert_optimized_source_build(self):
        builds = [event for event in self.events("compiler") if "self" in event["args"]]
        self.assertEqual(len(builds), 1)
        self.assertIn("-prod", builds[0]["args"])
        for name in ISOLATED:
            self.assertIsNone(builds[0]["env"][name], name)
        native_builds = [event for event in self.events("make") if "-C" in event["args"]]
        self.assertEqual(len(native_builds), 1)
        self.assertTrue(str(self.repo) in native_builds[0]["args"][-1])
        for name in ISOLATED:
            self.assertIsNone(native_builds[0]["env"][name], name)

    def test_source_build_is_optimized_and_isolates_only_build_flags(self):
        result = self.run_prepare("-j2", V_CI_SOURCE_BUILD="1", V_CI_CC="gcc")
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("curl"), [])
        build = next(event for event in self.events("compiler") if "self" in event["args"])
        self.assertEqual(build["args"][build["args"].index("-cc") + 1], "gcc")
        caches = [event for event in self.events("make") if "cache-basic-modules" in event["args"]]
        self.assertEqual(caches[0]["args"], ["-j2", "cache-basic-modules"])
        self.assertEqual(caches[0]["env"]["VFLAGS"], self.env["VFLAGS"])

    def test_source_only_ignores_prebuilt_and_never_downloads(self):
        result = self.run_prepare(V_CI_SOURCE_BUILD="1", V_CI_PREBUILT=str(self.asset()))
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("curl"), [])

    def test_source_only_fetch_returns_no_binary_and_does_not_build(self):
        result = self.run_prepare("--fetch", V_CI_SOURCE_BUILD="1", V_CI_PREBUILT=str(self.asset()))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), "")
        self.assertEqual(self.events("curl") + self.events("make") + self.events("compiler"), [])
        self.assertEqual((self.repo / "v").read_bytes(), self.original)

    def test_verified_artifact_reuses_compiler_and_provisions_dependencies(self):
        result = self.run_prepare(V_CI_PREBUILT=str(self.asset()))
        self.assert_success(result)
        self.assertEqual(self.events("curl"), [])
        self.assertEqual([event["args"] for event in self.events("make")], [
            ["-j4", "latest_vc", "latest_tcc", "latest_legacy"],
            ["-j4", "cache-basic-modules"],
        ])
        compiler_args = [event["args"] for event in self.events("compiler")]
        self.assertFalse(any("self" in args for args in compiler_args))
        self.assertIn(["-new-compiler", "-gc", "none", "run", "cmd/tools/detect_tcc.v"], compiler_args)
        self.assertIn(["-new-compiler", "-gc", "none", "run", ".github/problem-matchers/register_all.vsh"], compiler_args)

    def test_wrong_checksum_commit_or_platform_cannot_replace_compiler(self):
        cases = [
            (None, "0" * 64),
            (None, "not-a-checksum"),
            (None, "f" * 63),
            ("f" * 40 + "-linux-x64", None),
            (SHA + "-macos-arm64", None),
        ]
        for name, digest in cases:
            with self.subTest(name=name, digest=digest):
                self.log.write_text("")
                result = self.run_prepare(V_CI_PREBUILT=str(self.asset(name, digest)))
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual((self.repo / "v").read_bytes(), self.original)
                self.assertEqual(self.events("make") + self.events("compiler") + self.events("curl"), [])

    def test_failed_production_build_preserves_bootstrap_compiler(self):
        result = self.run_prepare(V_CI_SOURCE_BUILD="1", MOCK_FAIL_BUILD="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.repo / "v").read_bytes(), self.original)
        self.assertEqual(list(self.repo.glob("vprod.build.*")), [])
        self.assertFalse(any("cache-basic-modules" in event["args"] for event in self.events("make")))

    def test_unusable_artifact_preserves_bootstrap_compiler(self):
        result = self.run_prepare(V_CI_PREBUILT=str(self.asset()), MOCK_FAIL_VERSION="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.repo / "v").read_bytes(), self.original)

    def test_missing_release_falls_back_to_optimized_source_build(self):
        result = self.run_prepare()
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(len(self.events("curl")), 1)
        expected = f"https://github.com/medvednikov/v2/releases/download/vprod-{SHA}/{SHA}-linux-x64"
        self.assertIn(expected, self.events("curl")[0]["args"])

    def test_available_release_reuses_the_verified_binary(self):
        result = self.run_prepare(MOCK_RELEASE=str(self.asset()))
        self.assert_success(result)
        self.assertEqual(len(self.events("curl")), 2)
        self.assertFalse(any("self" in event["args"] for event in self.events("compiler")))
        self.assertFalse(any("-C" in event["args"] for event in self.events("make")))

    def test_empty_checkout_bootstraps_before_optimized_build(self):
        (self.repo / "v").unlink()
        result = self.run_prepare("-j2", V_CI_SOURCE_BUILD="1")
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("make")[0]["args"], ["-j2"])

    def test_incompatible_libc_never_downloads_a_linux_binary(self):
        result = self.run_prepare(MOCK_LIBC="glibc 2.31")
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("curl"), [])

    def test_platform_selection_rejects_incompatible_hosts(self):
        for operating_system, arch, libc, macos, expected in [
            ("Linux", "x86_64", "glibc 2.35", "", "linux-x64"),
            ("Linux", "x86_64", "glibc 2.31", "", ""),
            ("Linux", "x86_64", "musl", "", ""),
            ("Linux", "aarch64", "glibc 2.35", "", ""),
            ("Darwin", "arm64", "", "15.0", "macos-arm64"),
            ("Darwin", "x86_64", "", "15.0", "macos-x64"),
            ("Darwin", "arm64", "", "14.0", ""),
            ("FreeBSD", "amd64", "", "", ""),
        ]:
            with self.subTest(os=operating_system, arch=arch, libc=libc, macos=macos):
                result = self.run_prepare("--platform", MOCK_OS=operating_system,
                                          MOCK_ARCH=arch, MOCK_LIBC=libc, MOCK_MACOS=macos)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.strip(), expected)


if __name__ == "__main__":
    unittest.main()
