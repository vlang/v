"""Offline release-recipe checks; workflow parsing requires PyYAML.

Run: python3 -m unittest discover -s .github/scripts/tests -v
These check source structure and shell inputs, not Docker/Nix execution.
"""

import importlib.util
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

import yaml


SCRIPTS = Path(__file__).resolve().parents[1]
ROOT = SCRIPTS.parents[1]
SPEC = importlib.util.spec_from_file_location("prepare_docker_release", SCRIPTS / "prepare_docker_release.py")
prepare = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(prepare)
FIXTURE = json.loads((Path(__file__).parent / "fixtures/docker-release-recipes.json").read_text())
BOOTSTRAP = {
    "schema": 1, "selector": "v-source-time-v1", "tag": "0.5.2",
    "v_source_sha": "7647ce1c6fad63b5578bc07883139906de74b2f8", "v_source_epoch": 1783896439,
    "vc_history_head": "953311cd7932af81c26b0f83c88d3cc3e58dbf3b",
    "vc_revision": "7eb8c54a3843e5107d5af06d7a8c3e928f322475", "vc_epoch": 1783888081,
    "vc_source_sha": "45ae01d23168b6372f734eeb38a77360bbcf184a",
    "vc_c_sha256": "c68b70a9ba74b9387c1ae08df43cc620f40d8f78dee925fc229791c7f9d96e7d",
}


def without_bootstrap(text):
    # Check unchanged recipe behavior independently of the added bootstrap steps.
    return (text.replace(prepare.BOOTSTRAP_ARGS, "").replace(prepare.BOOTSTRAP_INSTALL, "")
            .replace(prepare.BOOTSTRAP_VERIFY, "").replace("make -o latest_vc ", "make "))


def instructions(text):
    """Join the continuations used by these exact fixtures, ignoring comments."""
    pending = ""
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        pending += line.strip()
        if pending.endswith("\\"):
            pending = pending[:-1] + " "
            continue
        yield pending.split(None, 1)
        pending = ""
    if pending:
        raise AssertionError("unterminated Dockerfile continuation")


def stage_graph(text, build_args):
    """Model ARG/ENV inheritance and FROM/COPY edges for the pinned recipes.

    This is a fixture-level structural check, not a replacement Docker parser.
    """
    global_args, named, stages = {}, {}, []
    current = None
    for instruction, value in instructions(text):
        if instruction == "ARG":
            name, separator, default = value.partition("=")
            args = global_args if current is None else current["args"]
            args[name] = build_args.get(name, default if separator else args.get(name, global_args.get(name)))
        elif instruction == "FROM":
            parts = value.split()
            base = re.sub(r"\$\{([^}]+)\}", lambda m: global_args[m[1]], parts[0])
            parent = named.get(base)
            current = {
                "base": base,
                "name": parts[2] if len(parts) == 3 else None,
                "args": dict(parent["args"]) if parent else {},
                "env": dict(parent["env"]) if parent else {},
                "runs": [],
                "copies": [],
            }
            if current["name"]:
                if current["name"] in named:
                    raise AssertionError("duplicate stage name")
                named[current["name"]] = current
            stages.append(current)
        elif instruction == "ENV":
            for assignment in shlex.split(value):
                name, _, setting = assignment.partition("=")
                current["env"][name] = setting
        elif instruction == "RUN":
            current["runs"].append((value, dict(current["args"])))
        elif instruction == "COPY" and value.startswith("--from="):
            source = value.split()[0].partition("=")[2]
            if source not in named:
                raise AssertionError(f"unresolved COPY stage {source}")
            current["copies"].append(source)
    return stages


class RecipePreparationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.source = self.root / "source"
        (self.root / "bootstrap.json").write_text(json.dumps(BOOTSTRAP))
        directory = self.source / "docker/vlang"
        directory.mkdir(parents=True)
        for name, text in FIXTURE["recipes"].items():
            (directory / name).write_bytes(text.encode())

    def test_all_seven_exact_recipes_are_deterministic_and_inputs_unchanged(self):
        self.assertEqual(FIXTURE["revision"], prepare.DOCKER_REVISION)
        first = prepare.prepare(self.source, self.root / "first", BOOTSTRAP, "0.5.2")
        second = prepare.prepare(self.source, self.root / "second", BOOTSTRAP, "0.5.2")
        self.assertEqual(first, second)
        self.assertEqual(set(first), set(FIXTURE["recipes"]) - {"Dockerfile.nix.build"})
        self.assertEqual(len(first), 7)
        self.assertEqual(prepare.read_sources(self.source), FIXTURE["recipes"])
        for name, text in first.items():
            self.assertEqual((self.root / "first" / name).read_text(), text)
            self.assertNotIn("@@", text)
            self.assertNotRegex(text, r"(?m)^FROM .*vlang:nix-build")
        for name in ("Dockerfile.alpine", "Dockerfile.debian", "Dockerfile.static"):
            self.assertEqual(without_bootstrap(first[name]), FIXTURE["recipes"][name])
        for variant in ("alpine", "debian"):
            name = f"Dockerfile.{variant}.dev-full"
            self.assertEqual(without_bootstrap(first[name]), FIXTURE["recipes"][name].replace("@@USERN@@", "${USER}"))

    def test_all_stage_dependencies_and_release_arg_scopes(self):
        recipes = prepare.render_recipes(FIXTURE["recipes"], BOOTSTRAP)
        for tag in ("0.5.2", "weekly.2026.42"):
            for name, text in recipes.items():
                with self.subTest(tag=tag, recipe=name):
                    stages = stage_graph(text, {"USER": "thevlang", "V_REV": tag,
                        "V_SOURCE_SHA": BOOTSTRAP["v_source_sha"], "VC_REV": BOOTSTRAP["vc_revision"],
                        "VC_C_SHA256": BOOTSTRAP["vc_c_sha256"]})
                    fetches = [(run, args) for stage in stages for run, args in stage["runs"] if "git fetch" in run or "git -C /opt/vlang fetch" in run]
                    self.assertEqual(len(fetches), 1)
                    run, args = fetches[0]
                    self.assertEqual(args["V_REV"], tag)
                    self.assertIn('origin "${V_REV}"', run)
                    self.assertIn("checkout --quiet FETCH_HEAD", run)
                    self.assertEqual(args["V_SOURCE_SHA"], BOOTSTRAP["v_source_sha"])
                    self.assertEqual(args["VC_REV"], BOOTSTRAP["vc_revision"])
                    self.assertEqual(args["VC_C_SHA256"], BOOTSTRAP["vc_c_sha256"])
                    if name.startswith("Dockerfile.nix"):
                        expected = ["nixos/nix:latest", prepare.TOOLCHAIN_STAGE]
                        if name == "Dockerfile.nix":
                            expected.append(prepare.TOOLCHAIN_STAGE)
                        self.assertEqual([stage["base"] for stage in stages], expected)
                        for stage in stages:
                            self.assertEqual(stage["env"]["PATH"], prepare.TOOLCHAIN_PATH)
                        package_run = stages[0]["runs"][0][0]
                        packages = package_run.split("-iA", 1)[1].split()
                        self.assertIn("gnumake", packages)
                        self.assertNotIn("make", packages)
                        self.assertIn("make -o latest_vc VFLAGS='-cc gcc' && test", run)
                        self.assertIn("v -version", run)
                    elif name == "Dockerfile.static":
                        self.assertEqual([stage["base"] for stage in stages], ["alpine:3.23", "scratch"])
                        self.assertIn("CFLAGS='-static'", run)
                        self.assertIn("./v -version", run)
                    else:
                        variant = name.split(".")[1]
                        expected = [f"thevlang/vlang:{variant}-build"]
                        if not name.endswith("dev-full"):
                            expected.append(f"thevlang/vlang:{variant}-base")
                        self.assertEqual([stage["base"] for stage in stages], expected)

    def test_missing_or_changed_input_fails_before_output_creation(self):
        for name, original in FIXTURE["recipes"].items():
            path = self.source / "docker/vlang" / name
            for data in (None, original.encode() + b"\n"):
                with self.subTest(recipe=name, missing=data is None):
                    path.unlink()
                    if data is not None:
                        path.write_bytes(data)
                    with self.assertRaises(prepare.PreparationError):
                        prepare.prepare(self.source, self.root / "rejected", BOOTSTRAP, "0.5.2")
                    self.assertFalse((self.root / "rejected").exists())
                    path.write_bytes(original.encode())

    def test_known_replacement_and_residual_guards(self):
        mutations = [
            ("Dockerfile.alpine.dev-full", "@@USERN@@", "${USER}"),
            ("Dockerfile.debian.dev-full", "@@USERN@@", "@@USERN@@@@USERN@@"),
            ("Dockerfile.nix", "FROM ${USER}/vlang:nix-build AS runtime", "FROM scratch AS runtime"),
            ("Dockerfile.nix.dev-full", "FROM ${USER}/vlang:nix-build", "FROM scratch"),
            ("Dockerfile.nix.build", "FROM nixos/nix:latest", "FROM nixos/nix:other"),
            ("Dockerfile.nix.build", "    make \\\n", "    other \\\n"),
            ("Dockerfile.nix.build", "ENV PATH=", "ENV OTHER_PATH="),
            ("Dockerfile.static", "origin \"${V_REV}\"", "origin master"),
            ("Dockerfile.static", "checkout --quiet FETCH_HEAD", "checkout master"),
            ("Dockerfile.static", "FROM scratch", "FROM scratch\n# @@EXTRA@@"),
            ("Dockerfile.static", "FROM scratch", "FROM ${USER}/vlang:nix-build"),
        ]
        for name, old, new in mutations:
            with self.subTest(recipe=name, pattern=old):
                sources = dict(FIXTURE["recipes"])
                self.assertIn(old, sources[name])
                sources[name] = sources[name].replace(old, new)
                with self.assertRaises(prepare.PreparationError):
                    prepare.render_recipes(sources, BOOTSTRAP)

    def test_output_reuse_is_rejected_without_modifying_existing_files(self):
        output = self.root / "output"
        prepare.prepare(self.source, output, BOOTSTRAP, "0.5.2")
        original = (output / "Dockerfile.static").read_bytes()
        with self.assertRaisesRegex(prepare.PreparationError, "output already exists"):
            prepare.prepare(self.source, output, BOOTSTRAP, "0.5.2")
        self.assertEqual((output / "Dockerfile.static").read_bytes(), original)

    def test_cli_prepares_actual_fixture_and_reports_drift(self):
        command = [sys.executable, str(SCRIPTS / "prepare_docker_release.py"), "--source", str(self.source), "--output", str(self.root / "cli"), "--release-tag", "0.5.2", "--bootstrap-manifest", str(self.root / "bootstrap.json")]
        result = subprocess.run(command, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Prepared 7 release recipes", result.stdout)
        (self.source / "docker/vlang/Dockerfile.nix").write_text("FROM scratch\n")
        result = subprocess.run(command, capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertIn("does not match vlang/docker@", result.stderr)

    def test_each_recipe_pins_vc_and_retains_original_build_flags(self):
        recipes = prepare.render_recipes(FIXTURE["recipes"], BOOTSTRAP)
        for name, text in recipes.items():
            with self.subTest(recipe=name):
                self.assertEqual(text.count("ARG V_SOURCE_SHA\nARG VC_REV\nARG VC_C_SHA256\n"), 1)
                self.assertIn('test "$(git -C /opt/vlang rev-parse HEAD)" = "${V_SOURCE_SHA}"', text)
                self.assertIn('git -C /opt/vlang/vc fetch --quiet --depth 1 origin "${VC_REV}"', text)
                self.assertIn('git -C /opt/vlang/vc checkout --quiet --detach FETCH_HEAD', text)
                self.assertEqual(text.count('test "$(git -C /opt/vlang/vc rev-parse HEAD)" = "${VC_REV}"'), 2)
                self.assertEqual(text.count('"${VC_C_SHA256}" | sha256sum -c -'), 2)
                self.assertEqual(text.count("make -o latest_vc "), 1)
                self.assertNotIn("local=1", text)
                original_make = next(line.strip() for line in FIXTURE["recipes"][name].splitlines()
                                     if line.startswith("    make "))
                expected_flags = original_make.split(" &&", 1)[0]
                self.assertIn(expected_flags.replace("make ", "make -o latest_vc ", 1), text)
                for instruction, value in instructions(text):
                    if instruction == "RUN":
                        result = subprocess.run(["sh", "-n"], input=value, text=True, capture_output=True)
                        self.assertEqual(result.returncode, 0, result.stderr)

    def test_manifest_drift_or_wrong_tag_fails_before_any_recipe_output(self):
        manifest = dict(BOOTSTRAP, tag="weekly.different")
        with self.assertRaisesRegex(prepare.PreparationError, "requested release tag"):
            prepare.prepare(self.source, self.root / "drift", manifest, "0.5.2")
        self.assertFalse((self.root / "drift").exists())


class HistoricalBootstrapTests(unittest.TestCase):
    epoch = 1700000000

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.v = self.init_repo("v")
        self.vc = self.init_repo("vc")
        makefile = "all: latest_vc latest_tcc\nlatest_vc:\n\t@echo update-vc\nlatest_tcc:\n\t@echo update-tcc\n"
        self.base = self.record(self.v, {"GNUmakefile": makefile}, "compiler", self.epoch)
        self.source = self.record(self.v, {"CHANGELOG.md": "release\n"}, "release", self.epoch + 200)
        self.call(self.v, "tag", "0.5.2")
        self.call(self.v, "tag", "weekly.fixture")
        self.call(self.v, "tag", "-a", "weekly.annotated", "-m", "late tag", epoch=self.epoch + 10000)
        self.first = self.snapshot(self.base, self.epoch + 100, "historical")
        self.record(self.vc, {"v_win.c": "windows only\n"}, "windows only", self.epoch + 190)
        self.newer = self.snapshot(self.source, self.epoch + 201, "newer than source")

    def call(self, repo, *args, epoch=None):
        env = dict(os.environ)
        if epoch is not None:
            env.update(GIT_AUTHOR_DATE=f"@{epoch} +0000", GIT_COMMITTER_DATE=f"@{epoch} +0000")
        return subprocess.check_output(["git", "-C", str(repo), *args], env=env, text=True, stderr=subprocess.PIPE).strip()

    def init_repo(self, name):
        path = self.root / name
        path.mkdir()
        self.call(path, "init", "--quiet", "--initial-branch=master")
        self.call(path, "config", "user.name", "Bootstrap test")
        self.call(path, "config", "user.email", "bootstrap@example.invalid")
        return path

    def record(self, repo, files, subject, epoch):
        for name, content in files.items():
            (repo / name).write_text(content)
        self.call(repo, "add", "--all")
        self.call(repo, "commit", "--quiet", "-m", subject, epoch=epoch)
        return self.call(repo, "rev-parse", "HEAD")

    def snapshot(self, generated, epoch, body, header=None, subject=None):
        return self.record(self.vc, {"v.c": f'#define V_COMMIT_HASH "{header or generated}"\n{body}\n'},
                           subject or f"[v:master] {generated} - generated", epoch)

    def test_numeric_weekly_and_annotated_tags_use_peeled_source_time(self):
        for tag in ("0.5.2", "weekly.fixture", "weekly.annotated"):
            with self.subTest(tag=tag):
                result = prepare.resolve_bootstrap(tag, self.v, self.vc)
                self.assertEqual(result["tag"], tag)
                self.assertEqual(result["v_source_sha"], self.source)
                self.assertEqual(result["v_source_epoch"], self.epoch + 200)
                self.assertEqual(result["vc_revision"], self.first)
                self.assertNotEqual(result["vc_revision"], self.newer)
                self.assertEqual(result["vc_source_sha"], self.base)
                self.assertEqual(result["vc_history_head"], self.newer)
                self.assertEqual(result, prepare.resolve_bootstrap(tag, self.v, self.vc))

    def test_timestamp_ties_have_stable_full_sha_order(self):
        a = self.snapshot(self.base, self.epoch + 150, "tie A")
        b = self.snapshot(self.base, self.epoch + 150, "tie B")
        self.assertEqual(prepare.resolve_bootstrap("0.5.2", self.v, self.vc)["vc_revision"], max(a, b))

    def test_short_generator_metadata_is_uniquely_resolved_to_full_sha(self):
        self.snapshot(self.base[:7], self.epoch + 190, "short metadata")
        result = prepare.resolve_bootstrap("0.5.2", self.v, self.vc)
        self.assertEqual(result["vc_source_sha"], self.base)

    def test_missing_snapshot_tag_or_make_contract_never_falls_back_to_current(self):
        self.call(self.v, "tag", "0.0.1", self.base)
        with self.assertRaisesRegex(prepare.PreparationError, "no historical"):
            prepare.resolve_bootstrap("0.0.1", self.v, self.vc)
        with self.assertRaises(prepare.PreparationError):
            prepare.resolve_bootstrap("weekly.missing", self.v, self.vc)
        self.record(self.v, {"GNUmakefile": "all:\n\t@echo unsupported\n"}, "new make", self.epoch + 300)
        self.call(self.v, "tag", "weekly.unsupported")
        with self.assertRaisesRegex(prepare.PreparationError, "latest_vc contract"):
            prepare.resolve_bootstrap("weekly.unsupported", self.v, self.vc)

    def test_nonancestor_or_mismatched_generator_metadata_is_rejected(self):
        other = self.call(self.v, "commit-tree", self.base + "^{tree}", "-m", "unrelated", epoch=self.epoch + 50)
        self.snapshot(other, self.epoch + 180, "not an ancestor")
        with self.assertRaises(prepare.PreparationError):
            prepare.resolve_bootstrap("0.5.2", self.v, self.vc)
        self.snapshot(self.base, self.epoch + 190, "wrong header", header=self.source)
        with self.assertRaisesRegex(prepare.PreparationError, "disagree"):
            prepare.resolve_bootstrap("0.5.2", self.v, self.vc)
        self.snapshot(self.base, self.epoch + 199, "malformed", subject="not generator metadata")
        with self.assertRaisesRegex(prepare.PreparationError, "generator metadata"):
            prepare.resolve_bootstrap("0.5.2", self.v, self.vc)

    def test_tags_are_validated_before_git_or_history_creation(self):
        for tag in ("master", "", "0.5.2;echo bad", "weekly.x\noutput=value", "weekly.a/b", "weekly..x", "weekly.x.lock", "weekly.x.", "weekly." + "a" * 114):
            with self.subTest(tag=tag), mock.patch.object(prepare, "git") as command:
                with self.assertRaises(prepare.PreparationError):
                    prepare.resolve_bootstrap(tag, self.v, self.vc)
                with self.assertRaises(prepare.PreparationError):
                    prepare.fetch_histories(tag, self.root / "rejected")
                command.assert_not_called()
                self.assertFalse((self.root / "rejected").exists())

    def test_history_fetch_uses_only_literal_urls_and_tag_refs_in_argv(self):
        with mock.patch.object(prepare, "git", return_value=b"") as command:
            prepare.fetch_histories("weekly.fixture", self.root / "fetch")
        calls = [call.args for call in command.call_args_list]
        remotes = [args[1:] for args in calls if args[1] == "remote"]
        self.assertEqual(remotes, [("remote", "add", "origin", "https://github.com/vlang/v"),
                                   ("remote", "add", "origin", "https://github.com/vlang/vc")])
        fetches = [args[1:] for args in calls if args[1] == "fetch"]
        self.assertEqual(fetches[0][-1], "refs/tags/weekly.fixture:refs/tags/weekly.fixture")
        self.assertEqual(fetches[1][-1], "refs/heads/master:refs/heads/master")
        with mock.patch.object(prepare.subprocess, "run") as run:
            run.return_value.returncode, run.return_value.stdout = 0, b"ok"
            self.assertEqual(prepare.git(self.v, "show", self.source), b"ok")
            self.assertEqual(run.call_args.args[0], ["git", "-C", str(self.v), "show", self.source])
            self.assertNotIn("shell", run.call_args.kwargs)

    def test_manifest_schema_duplicate_fields_and_output_grammar(self):
        bad = [("schema", True), ("schema", 2), ("selector", "floating"),
               ("tag", "weekly.x\nother=value"), ("v_source_sha", "a" * 39),
               ("vc_history_head", "A" * 40), ("vc_revision", "master"),
               ("vc_source_sha", "a" * 40 + "\n"), ("vc_c_sha256", "a" * 63),
               ("vc_epoch", True), ("vc_epoch", "1"), ("vc_epoch", -1),
               ("v_source_epoch", 1.5), ("vc_epoch", BOOTSTRAP["v_source_epoch"] + 1)]
        for key, value in bad:
            with self.subTest(field=key, value=value):
                manifest = dict(BOOTSTRAP, **{key: value})
                with self.assertRaises(prepare.PreparationError):
                    prepare.github_outputs(manifest)
        for manifest in ({}, dict(BOOTSTRAP, extra=True)):
            with self.assertRaises(prepare.PreparationError):
                prepare.validate_manifest(manifest)
        path = self.root / "manifest.json"
        path.write_text(json.dumps(BOOTSTRAP).replace('"schema": 1', '"schema": 1, "schema": 1'))
        with self.assertRaisesRegex(prepare.PreparationError, "duplicate"):
            prepare.read_manifest(path)
        output = prepare.github_outputs(BOOTSTRAP)
        rows = dict(line.split("=", 1) for line in output.splitlines())
        self.assertEqual(set(rows), {"tag", "v_source_sha", "vc_revision", "vc_c_sha256", "manifest"})
        self.assertEqual(json.loads(rows["manifest"]), BOOTSTRAP)

    def test_make_assume_old_preserves_tcc_and_legacy_and_skips_vc(self):
        (self.root / "Makefile").write_text(
            "all: latest_vc latest_tcc latest_legacy\n\t@echo compiler >> calls\n"
            ".PHONY: all latest_vc latest_tcc latest_legacy\n"
            "latest_vc: missing-vc\n\t@echo forbidden-pull >> calls; false\n"
            "missing-vc:\n\t@echo forbidden-clone >> calls; false\n"
            "latest_tcc:\n\t@echo tcc >> calls\nlatest_legacy:\n\t@echo legacy >> calls\n")
        result = subprocess.run(["make", "-o", "latest_vc"], cwd=self.root, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual((self.root / "calls").read_text().splitlines(), ["tcc", "legacy", "compiler"])
        result = subprocess.run(["make"], cwd=self.root, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)


class WorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = yaml.load((ROOT / ".github/workflows/hub_docker_ci.yml").read_text(), Loader=yaml.BaseLoader)
        cls.job = cls.workflow["jobs"]["publish-release-images"]
        cls.steps = cls.job["steps"]

    def test_one_validated_resolution_manifest_is_shared_by_matrix(self):
        self.assertEqual(set(self.workflow["jobs"]), {"resolve-bootstrap", "publish-release-images"})
        self.assertEqual(self.job["needs"], "resolve-bootstrap")
        resolution = self.workflow["jobs"]["resolve-bootstrap"]
        self.assertEqual(set(resolution["outputs"]), {"tag", "manifest", "v_source_sha", "vc_revision", "vc_c_sha256"})
        for field, value in resolution["outputs"].items():
            self.assertEqual(value, "${{ steps.bootstrap.outputs." + field + " }}")
        step = next(s for s in resolution["steps"] if s.get("id") == "bootstrap")
        self.assertEqual(step["env"]["V_RELEASE_TAG"], "${{ inputs.tag || github.ref_name }}")
        self.assertIn('--resolve-bootstrap "$V_RELEASE_TAG"', step["run"])
        self.assertIn('--github-output "$GITHUB_OUTPUT"', step["run"])
        self.assertEqual(self.job["env"]["BOOTSTRAP_MANIFEST"], "${{ needs.resolve-bootstrap.outputs.manifest }}")
        preparation = next(s for s in self.steps if s.get("name") == "Prepare all release recipes")
        self.assertIn('--release-tag "$RESOLVED_TAG"', preparation["run"])
        self.assertIn('--bootstrap-manifest "$RUNNER_TEMP/bootstrap.json"', preparation["run"])

    def test_pin_preparation_paths_and_all_seven_outputs(self):
        self.assertEqual(self.job["strategy"]["matrix"]["os"], ["debian", "alpine", "nix", "static"])
        self.assertEqual(self.workflow["env"]["PLATFORMS"], "linux/amd64,linux/arm64")
        checkouts = [step for step in self.steps if step.get("uses", "").startswith("actions/checkout@")]
        self.assertEqual(len(checkouts), 2)
        self.assertNotIn("repository", checkouts[0]["with"])
        self.assertEqual(checkouts[1]["with"]["repository"], "vlang/docker")
        self.assertEqual(checkouts[1]["with"]["ref"], prepare.DOCKER_REVISION)
        self.assertEqual(checkouts[1]["with"]["path"], "docker-release-source")
        for checkout in checkouts:
            self.assertEqual(checkout["with"]["persist-credentials"], "false")
        preparation = next(step for step in self.steps if step.get("name") == "Prepare all release recipes")
        self.assertIn("python3 .github/scripts/prepare_docker_release.py", preparation["run"])
        self.assertIn("--source docker-release-source", preparation["run"])
        self.assertIn("--output docker-release-source/.release", preparation["run"])
        login = next(step for step in self.steps if step.get("uses", "").startswith("docker/login-action@"))
        self.assertLess(self.steps.index(preparation), self.steps.index(login))
        builds = [step for step in self.steps if step.get("uses", "").startswith("docker/build-push-action@")]
        self.assertEqual(len(builds), 2)
        self.assertNotIn("if", builds[0])
        self.assertEqual(builds[1]["if"], "matrix.os != 'static'")
        generated = set(prepare.render_recipes(FIXTURE["recipes"], BOOTSTRAP))
        selected = set()
        for index, build in enumerate(builds):
            settings = build["with"]
            self.assertEqual(settings["context"], "docker-release-source")
            self.assertEqual(settings["platforms"], "${{ env.PLATFORMS }}")
            self.assertEqual(settings["build-args"].splitlines(), ["USER=thevlang", "V_REV=${{ steps.tag.outputs.tag }}", "V_SOURCE_SHA=${{ needs.resolve-bootstrap.outputs.v_source_sha }}", "VC_REV=${{ needs.resolve-bootstrap.outputs.vc_revision }}", "VC_C_SHA256=${{ needs.resolve-bootstrap.outputs.vc_c_sha256 }}"])
            self.assertIn("org.opencontainers.image.version=${{ steps.tag.outputs.tag }}", settings["labels"])
            self.assertIn("org.opencontainers.image.revision=${{ steps.tag.outputs.tag }}", settings["labels"])
            for variant in self.job["strategy"]["matrix"]["os"]:
                if index == 1 and variant == "static":
                    continue
                path = settings["file"].replace("${{ matrix.os }}", variant)
                self.assertTrue(path.startswith("docker-release-source/.release/"))
                selected.add(Path(path).name)
        self.assertEqual(selected, generated)

    def test_tag_validation_and_actual_shell_generated_tags(self):
        tag_step = next(step for step in self.steps if step.get("id") == "tag")
        tags_step = next(step for step in self.steps if step.get("id") == "gen_tags")
        self.assertEqual(tag_step["env"]["V_RELEASE_TAG"], "${{ needs.resolve-bootstrap.outputs.tag }}")
        self.assertEqual(tags_step["env"]["V_RELEASE_TAG"], "${{ steps.tag.outputs.tag }}")
        for tag in ("0.5.2", "weekly.2026.42", "weekly." + "a" * 113):
            status, output = self.run_shell(tag_step["run"], V_RELEASE_TAG=tag)
            self.assertEqual(status, 0)
            self.assertEqual(output, f"tag={tag}\n")
            for variant in ("debian", "alpine", "nix", "static"):
                status, output = self.run_shell(tags_step["run"], V_RELEASE_TAG=tag, IMAGE_VARIANT=variant)
                self.assertEqual(status, 0)
                actual = output.strip().removeprefix("TAGS=").split(",")
                expected = [f"thevlang/vlang:{variant}-{tag}"]
                if variant != "static":
                    expected.insert(0, f"thevlang/vlang:{variant}")
                if variant == "debian":
                    expected = ["thevlang/vlang:latest", "thevlang/vlang:debian", f"thevlang/vlang:{tag}", f"thevlang/vlang:debian-{tag}"]
                self.assertEqual(actual, expected)
                self.assertTrue(all(len(value.split(":")[1]) <= 128 for value in actual))
        for tag in ("", "master", "weekly.", "0.5.2;echo broken", "weekly.a/b", "weekly." + "a" * 114):
            status, output = self.run_shell(tag_step["run"], V_RELEASE_TAG=tag)
            self.assertNotEqual(status, 0)
            self.assertEqual(output, "")

    def test_all_workflow_shell_snippets_have_valid_syntax(self):
        for job in self.workflow["jobs"].values():
            for step in job["steps"]:
                if "run" in step:
                    self.assertNotIn("${{", step["run"])
                    result = subprocess.run(["bash", "-n"], input=step["run"], capture_output=True, text=True)
                    self.assertEqual(result.returncode, 0, result.stderr)

    def run_shell(self, script, **values):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "output"
            env = {**os.environ, **values, "GITHUB_OUTPUT": str(output)}
            result = subprocess.run(["bash", "-c", script], env=env, capture_output=True, text=True)
            return result.returncode, output.read_text() if output.exists() else ""


if __name__ == "__main__":
    unittest.main()
