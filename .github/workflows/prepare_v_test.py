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
    if args == ["LONG_BIT"]:
        print(os.environ["MOCK_LONG_BIT"])
    else:
        assert args == ["GNU_LIBC_VERSION"], args
        print(os.environ["MOCK_LIBC"])
elif tool == "sw_vers":
    print(os.environ.get("MOCK_MACOS", "15.0"))
elif tool == "curl":
    sys.exit("Compiler setup must not download a binary")
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
        self.log.touch()
        temporary_files = self.base / "runner temp"
        temporary_files.mkdir()
        self.env = dict(os.environ)
        for key in list(self.env):
            if key.startswith(("V_CI_", "MOCK_")) or key in (*ISOLATED, "TERMUX_VERSION", "PREFIX"):
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
            "MOCK_LONG_BIT": "64",
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
        folder = self.base / "workflow artifacts"
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

    def test_artifact_reuse_does_not_bootstrap_a_missing_compiler(self):
        (self.repo / "v").unlink()
        result = self.run_prepare(V_CI_PREBUILT=str(self.asset()))
        self.assert_success(result)
        self.assertEqual([event["args"] for event in self.events("make")], [
            ["-j4", "latest_vc", "latest_tcc", "latest_legacy"],
            ["-j4", "cache-basic-modules"],
        ])
        self.assertEqual(self.events("curl"), [])

    def test_macos_images_reuse_the_same_exact_commit_arm64_compiler(self):
        for version in ("15.0", "26.0"):
            with self.subTest(version=version):
                self.log.write_text("")
                (self.repo / "v").unlink()
                result = self.run_prepare(
                    MOCK_OS="Darwin", MOCK_ARCH="arm64", MOCK_MACOS=version,
                    V_CI_PREBUILT=str(self.asset(f"{SHA}-macos-arm64")),
                )
                self.assert_success(result)
                self.assertEqual([event["args"] for event in self.events("make")], [
                    ["-j4", "latest_vc", "latest_tcc", "latest_legacy"],
                    ["-j4", "cache-basic-modules"],
                ])
                self.assertFalse(any("self" in event["args"] for event in self.events("compiler")))

    def test_macos_artifacts_require_matching_commit_platform_and_checksum(self):
        for name, digest in [
            ("f" * 40 + "-macos-arm64", None),
            (f"{SHA}-macos-x64", None),
            (f"{SHA}-linux-x64", None),
            (f"{SHA}-macos-arm64", "0" * 64),
        ]:
            with self.subTest(name=name, digest=digest):
                self.log.write_text("")
                result = self.run_prepare(
                    MOCK_OS="Darwin", MOCK_ARCH="arm64", MOCK_MACOS="26.0",
                    V_CI_PREBUILT=str(self.asset(name, digest)),
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual((self.repo / "v").read_bytes(), self.original)
                self.assertEqual(self.events("make") + self.events("compiler"), [])

    def test_verified_termux_artifacts_reuse_compiler_for_each_architecture(self):
        for arch, platform in [("aarch64", "termux-arm64"), ("x86_64", "termux-x64")]:
            with self.subTest(arch=arch):
                self.log.write_text("")
                result = self.run_prepare(
                    TERMUX_VERSION="0.118.3", MOCK_ARCH=arch, MOCK_LIBC="bionic",
                    V_CI_PREBUILT=str(self.asset(f"{SHA}-{platform}")),
                )
                self.assert_success(result)
                self.assertEqual([event["args"] for event in self.events("make")], [
                    ["-j4", "latest_vc", "latest_tcc", "latest_legacy"],
                    ["-j4", "cache-basic-modules"],
                ])
                self.assertFalse(any("self" in event["args"] for event in self.events("compiler")))

    def test_termux_artifacts_require_matching_platform_and_checksum(self):
        for arch, version, platform, digest in [
            ("x86_64", "0.118.3", "linux-x64", None),
            ("aarch64", "0.118.3", "termux-x64", None),
            ("x86_64", "0.118.3", "termux-arm64", None),
            ("x86_64", "", "termux-x64", None),
            ("aarch64", "0.118.3", "termux-arm64", "0" * 64),
        ]:
            with self.subTest(arch=arch, version=version, platform=platform, digest=digest):
                self.log.write_text("")
                result = self.run_prepare(
                    TERMUX_VERSION=version, MOCK_ARCH=arch,
                    V_CI_PREBUILT=str(self.asset(f"{SHA}-{platform}", digest)),
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual((self.repo / "v").read_bytes(), self.original)
                self.assertEqual(self.events("make") + self.events("compiler") + self.events("curl"), [])

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

    def test_missing_artifact_or_checksum_cannot_replace_compiler(self):
        for missing_checksum in (False, True):
            with self.subTest(missing_checksum=missing_checksum):
                self.log.write_text("")
                asset = self.asset()
                missing = Path(str(asset) + ".sha256") if missing_checksum else asset
                missing.unlink()
                result = self.run_prepare(V_CI_PREBUILT=str(asset))
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

    def test_default_build_optimizes_source_without_fetching_a_binary(self):
        result = self.run_prepare()
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("curl"), [])

    def test_invalid_source_build_option_fails_before_modifying_checkout(self):
        result = self.run_prepare(V_CI_SOURCE_BUILD="true")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("V_CI_SOURCE_BUILD must be 0 or 1", result.stderr)
        self.assertEqual((self.repo / "v").read_bytes(), self.original)
        self.assertEqual(self.events(), [])

    def test_empty_checkout_bootstraps_before_optimized_build(self):
        (self.repo / "v").unlink()
        result = self.run_prepare("-j2", V_CI_SOURCE_BUILD="1")
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("make")[0]["args"], ["-j2"])

    def test_incompatible_libc_builds_native_optimized_compiler(self):
        result = self.run_prepare(MOCK_LIBC="glibc 2.31")
        self.assert_success(result)
        self.assert_optimized_source_build()
        self.assertEqual(self.events("curl"), [])

    def test_incompatible_libc_rejects_explicit_linux_artifact(self):
        result = self.run_prepare(MOCK_LIBC="glibc 2.31", V_CI_PREBUILT=str(self.asset()))
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.repo / "v").read_bytes(), self.original)
        self.assertEqual(self.events("make") + self.events("compiler") + self.events("curl"), [])

    def test_platform_selection_rejects_incompatible_hosts(self):
        for operating_system, arch, libc, macos, expected in [
            ("Linux", "x86_64", "glibc 2.35", "", "linux-x64"),
            ("Linux", "x86_64", "glibc 2.31", "", ""),
            ("Linux", "x86_64", "musl", "", ""),
            ("Linux", "x86_64", "bionic", "", ""),
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

    def test_termux_platform_selection_precedes_glibc_detection(self):
        for marker in [{"TERMUX_VERSION": "0.118.3"},
                       {"PREFIX": "/data/data/com.termux/files/usr"}]:
            for arch, expected in [("aarch64", "termux-arm64"),
                                   ("x86_64", "termux-x64"), ("armv7l", "")]:
                with self.subTest(marker=marker, arch=arch):
                    self.log.write_text("")
                    result = self.run_prepare("--platform", MOCK_ARCH=arch, **marker)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout.strip(), expected)
                    self.assertNotIn(["GNU_LIBC_VERSION"],
                                     [event["args"] for event in self.events("getconf")])

    def test_termux_32bit_or_unknown_userland_does_not_reuse_64bit_artifacts(self):
        for arch, platform in [("aarch64", "termux-arm64"), ("x86_64", "termux-x64")]:
            for word_size in ("32", "", "unknown"):
                with self.subTest(arch=arch, word_size=word_size):
                    self.log.write_text("")
                    env = {"TERMUX_VERSION": "0.118.3", "MOCK_ARCH": arch,
                           "MOCK_LONG_BIT": word_size}
                    result = self.run_prepare("--platform", **env)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout.strip(), "")
                    result = self.run_prepare(
                        V_CI_PREBUILT=str(self.asset(f"{SHA}-{platform}")), **env,
                    )
                    self.assertNotEqual(result.returncode, 0)
                    self.assertEqual((self.repo / "v").read_bytes(), self.original)
                    self.assertEqual(self.events("make") + self.events("compiler"), [])

    def test_termux_prefix_requires_literal_package_name(self):
        result = self.run_prepare("--platform", MOCK_LIBC="bionic",
                                  PREFIX="/data/data/comXtermux/files/usr")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), "")


if __name__ == "__main__":
    unittest.main()
