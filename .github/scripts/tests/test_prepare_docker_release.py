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

import yaml


SCRIPTS = Path(__file__).resolve().parents[1]
ROOT = SCRIPTS.parents[1]
SPEC = importlib.util.spec_from_file_location("prepare_docker_release", SCRIPTS / "prepare_docker_release.py")
prepare = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(prepare)
FIXTURE = json.loads((Path(__file__).parent / "fixtures/docker-release-recipes.json").read_text())


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
        directory = self.source / "docker/vlang"
        directory.mkdir(parents=True)
        for name, text in FIXTURE["recipes"].items():
            (directory / name).write_bytes(text.encode())

    def test_all_seven_exact_recipes_are_deterministic_and_inputs_unchanged(self):
        self.assertEqual(FIXTURE["revision"], prepare.DOCKER_REVISION)
        first = prepare.prepare(self.source, self.root / "first")
        second = prepare.prepare(self.source, self.root / "second")
        self.assertEqual(first, second)
        self.assertEqual(set(first), set(FIXTURE["recipes"]) - {"Dockerfile.nix.build"})
        self.assertEqual(len(first), 7)
        self.assertEqual(prepare.read_sources(self.source), FIXTURE["recipes"])
        for name, text in first.items():
            self.assertEqual((self.root / "first" / name).read_text(), text)
            self.assertNotIn("@@", text)
            self.assertNotRegex(text, r"(?m)^FROM .*vlang:nix-build")
        for name in ("Dockerfile.alpine", "Dockerfile.debian", "Dockerfile.static"):
            self.assertEqual(first[name], FIXTURE["recipes"][name])
        for variant in ("alpine", "debian"):
            name = f"Dockerfile.{variant}.dev-full"
            self.assertEqual(first[name], FIXTURE["recipes"][name].replace("@@USERN@@", "${USER}"))

    def test_all_stage_dependencies_and_release_arg_scopes(self):
        recipes = prepare.render_recipes(FIXTURE["recipes"])
        for tag in ("0.5.2", "weekly.2026.42"):
            for name, text in recipes.items():
                with self.subTest(tag=tag, recipe=name):
                    stages = stage_graph(text, {"USER": "thevlang", "V_REV": tag})
                    fetches = [(run, args) for stage in stages for run, args in stage["runs"] if "git fetch" in run or "git -C /opt/vlang fetch" in run]
                    self.assertEqual(len(fetches), 1)
                    run, args = fetches[0]
                    self.assertEqual(args["V_REV"], tag)
                    self.assertIn('origin "${V_REV}"', run)
                    self.assertIn("checkout --quiet FETCH_HEAD", run)
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
                        self.assertIn("make VFLAGS='-cc gcc' && v -version", run)
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
                        prepare.prepare(self.source, self.root / "rejected")
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
                    prepare.render_recipes(sources)

    def test_output_reuse_is_rejected_without_modifying_existing_files(self):
        output = self.root / "output"
        prepare.prepare(self.source, output)
        original = (output / "Dockerfile.static").read_bytes()
        with self.assertRaisesRegex(prepare.PreparationError, "output already exists"):
            prepare.prepare(self.source, output)
        self.assertEqual((output / "Dockerfile.static").read_bytes(), original)

    def test_cli_prepares_actual_fixture_and_reports_drift(self):
        command = [sys.executable, str(SCRIPTS / "prepare_docker_release.py"), "--source", str(self.source), "--output", str(self.root / "cli")]
        result = subprocess.run(command, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Prepared 7 release recipes", result.stdout)
        (self.source / "docker/vlang/Dockerfile.nix").write_text("FROM scratch\n")
        result = subprocess.run(command, capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertIn("does not match vlang/docker@", result.stderr)


class WorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = yaml.load((ROOT / ".github/workflows/hub_docker_ci.yml").read_text(), Loader=yaml.BaseLoader)
        cls.job = cls.workflow["jobs"]["publish-release-images"]
        cls.steps = cls.job["steps"]

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
        generated = set(prepare.render_recipes(FIXTURE["recipes"]))
        selected = set()
        for index, build in enumerate(builds):
            settings = build["with"]
            self.assertEqual(settings["context"], "docker-release-source")
            self.assertEqual(settings["platforms"], "${{ env.PLATFORMS }}")
            self.assertEqual(settings["build-args"].splitlines(), ["USER=thevlang", "V_REV=${{ steps.tag.outputs.tag }}"])
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
        self.assertEqual(tag_step["env"]["V_RELEASE_TAG"], "${{ inputs.tag || github.ref_name }}")
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
        for step in self.steps:
            if "run" in step:
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
