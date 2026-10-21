#!/usr/bin/env python3
"""Prepare release recipes from the reviewed vlang/docker snapshot.

This deliberately handles one pinned recipe version, rather than rewriting
arbitrary Dockerfiles. A companion update must review these hashes and edits.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess


DOCKER_REVISION = "4cfc69eb8ea27b1d1e3abf13dccc6b3ae0fd5046"
SOURCE_HASHES = {
    "Dockerfile.alpine": "86961a0b7385cacbfaa9a6a71bd4ac8e603cd2b5441661d9233ec15c557e1957",
    "Dockerfile.alpine.dev-full": "4c12456654961528f45aee811d715a7df0459b4d7ae5a3af63cba2c6e2f85ce1",
    "Dockerfile.debian": "86a0739b08f15747133ef12e70c928e51d00d74976add462522d642bce77f1d9",
    "Dockerfile.debian.dev-full": "5580401b18d897c2cd435df52c7a1012ededceedf7e2282f9b153db87bc491ac",
    "Dockerfile.nix": "bc7069ee5fdf1ff0d58e114ee6b23002a7e412ab0f7bf682ad4007c17d23a538",
    "Dockerfile.nix.build": "cc16ad890ac3c091f5caad891cce8c22569759889388ebdab297b0d73f365a7a",
    "Dockerfile.nix.dev-full": "85c6fb1216d291bc953ea186ca11f46510868a2180067ed6d061ff860280e75c",
    "Dockerfile.static": "2625a47cad53dc1927a7f8c292214cac90b6752194b1f823475bb4b7acb62b20",
}
TOOLCHAIN_STAGE = "vlang-nix-toolchain"
TOOLCHAIN_PATH = "/opt/vlang:/root/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/bin:/bin"
MANIFEST_KEYS = {
    "schema", "selector", "tag", "v_source_sha", "v_source_epoch", "vc_history_head",
    "vc_revision", "vc_epoch", "vc_source_sha", "vc_c_sha256",
}
BOOTSTRAP_ARGS = "ARG V_SOURCE_SHA\nARG VC_REV\nARG VC_C_SHA256\n"
BOOTSTRAP_INSTALL = r'''    test "$(git -C /opt/vlang rev-parse HEAD)" = "${V_SOURCE_SHA}" && \
    test ! -e /opt/vlang/vc && \
    git init --quiet /opt/vlang/vc && \
    git -C /opt/vlang/vc remote add origin https://github.com/vlang/vc && \
    git -C /opt/vlang/vc fetch --quiet --depth 1 origin "${VC_REV}" && \
    git -C /opt/vlang/vc checkout --quiet --detach FETCH_HEAD && \
    test "$(git -C /opt/vlang/vc rev-parse HEAD)" = "${VC_REV}" && \
    printf '%s  /opt/vlang/vc/v.c\n' "${VC_C_SHA256}" | sha256sum -c - && \
'''
BOOTSTRAP_VERIFY = r'''test "$(git -C /opt/vlang/vc rev-parse HEAD)" = "${VC_REV}" && \
    printf '%s  /opt/vlang/vc/v.c\n' "${VC_C_SHA256}" | sha256sum -c - && \
    '''


class PreparationError(ValueError):
    """The pinned input or an expected replacement does not match."""


def validate_tag(tag):
    if (not isinstance(tag, str) or len(tag) > 120
            or re.fullmatch(r"(?:[0-9]+\.[0-9]+\.[0-9]+|weekly\.[A-Za-z0-9_.-]+)", tag) is None
            or ".." in tag or tag.endswith((".", ".lock"))):
        raise PreparationError("expected a release or weekly V tag that fits the image tags")


def validate_manifest(manifest, tag=None):
    if not isinstance(manifest, dict) or set(manifest) != MANIFEST_KEYS:
        raise PreparationError("invalid bootstrap manifest fields")
    if type(manifest["schema"]) is not int or manifest["schema"] != 1 or manifest["selector"] != "v-source-time-v1":
        raise PreparationError("unsupported bootstrap manifest schema/selector")
    validate_tag(manifest["tag"])
    if tag is not None:
        validate_tag(tag)
        if manifest["tag"] != tag:
            raise PreparationError("bootstrap manifest does not match requested release tag")
    for field in ("v_source_sha", "vc_history_head", "vc_revision", "vc_source_sha", "vc_c_sha256"):
        length = 64 if field == "vc_c_sha256" else 40
        value = manifest[field]
        if not isinstance(value, str) or re.fullmatch(r"[0-9a-f]{" + str(length) + "}", value) is None:
            raise PreparationError(f"invalid bootstrap {field}")
    for field in ("v_source_epoch", "vc_epoch"):
        if type(manifest[field]) is not int or not 0 <= manifest[field] <= 2**63 - 1:
            raise PreparationError(f"invalid bootstrap {field}")
    if manifest["vc_epoch"] > manifest["v_source_epoch"]:
        raise PreparationError("bootstrap is newer than requested V source")
    return manifest


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise PreparationError("duplicate bootstrap manifest field")
        result[key] = value
    return result


def read_manifest(path, tag=None):
    try:
        data = json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=unique_object)
    except (ValueError, UnicodeError) as error:
        raise PreparationError(f"invalid bootstrap manifest: {error}") from error
    return validate_manifest(data, tag)


def git(repo, *args):
    result = subprocess.run(["git", "-C", str(repo), *args], capture_output=True, timeout=600)
    if result.returncode:
        raise PreparationError(f"git {args[0]} failed: {result.stderr.decode('utf-8', errors='replace')}")
    return result.stdout


def commit(repo, ref):
    value = git(repo, "rev-parse", "--verify", ref + "^{commit}").decode("ascii").strip()
    if re.fullmatch(r"[0-9a-f]{40}", value) is None:
        raise PreparationError("git did not resolve a full commit SHA")
    return value


def resolve_bootstrap(tag, v_repo, vc_repo):
    validate_tag(tag)
    source = commit(v_repo, "refs/tags/" + tag)
    raw_epoch = git(v_repo, "show", "-s", "--format=%ct", source).decode("ascii").strip()
    if re.fullmatch(r"[0-9]+", raw_epoch) is None:
        raise PreparationError("invalid V source timestamp")
    source_epoch = int(raw_epoch)
    history_head = commit(vc_repo, "refs/heads/master")
    history = git(vc_repo, "log", "--format=%ct%x09%H%x09%s", history_head, "--", "v.c").decode("utf-8")
    candidates = []
    for line in history.splitlines():
        fields = line.split("\t", 2)
        if (len(fields) != 3 or re.fullmatch(r"[0-9]+", fields[0]) is None
                or re.fullmatch(r"[0-9a-f]{40}", fields[1]) is None):
            raise PreparationError("invalid VC history record")
        epoch = int(fields[0])
        if epoch <= source_epoch:
            candidates.append((epoch, fields[1], fields[2]))
    if not candidates:
        raise PreparationError("no historical v.c snapshot for requested source")
    epoch, revision, subject = max(candidates, key=lambda row: (row[0], row[1]))
    subject_match = re.fullmatch(r"\[v:master\] ([0-9a-f]{7,40}) - [^\x00-\x1f\x7f]+", subject)
    data = git(vc_repo, "show", revision + ":v.c")
    header = data.split(b"\n", 1)[0].decode("ascii")
    header_match = re.fullmatch(r'#define V_COMMIT_HASH "([0-9a-f]{7,40})"', header)
    if subject_match is None or header_match is None:
        raise PreparationError("invalid VC generator metadata")
    generated = commit(v_repo, subject_match[1])
    if commit(v_repo, header_match[1]) != generated:
        raise PreparationError("VC subject and first source hash disagree")
    git(v_repo, "merge-base", "--is-ancestor", generated, source)
    makefile = git(v_repo, "show", source + ":GNUmakefile").decode("utf-8")
    if (re.search(r"^all:[^\n]*\blatest_vc\b", makefile, re.MULTILINE) is None
            or re.search(r"^latest_vc:", makefile, re.MULTILINE) is None):
        raise PreparationError("requested source lacks the GNU Make latest_vc contract")
    return validate_manifest({
        "schema": 1, "selector": "v-source-time-v1", "tag": tag,
        "v_source_sha": source, "v_source_epoch": source_epoch,
        "vc_history_head": history_head, "vc_revision": revision, "vc_epoch": epoch,
        "vc_source_sha": generated, "vc_c_sha256": hashlib.sha256(data).hexdigest(),
    })


def fetch_histories(tag, directory):
    validate_tag(tag)
    directory.mkdir(parents=True, exist_ok=False)
    for name, url, ref in (
        ("v", "https://github.com/vlang/v", "refs/tags/" + tag),
        ("vc", "https://github.com/vlang/vc", "refs/heads/master"),
    ):
        path = directory / name
        path.mkdir()
        git(path, "init", "--quiet", "--bare")
        git(path, "remote", "add", "origin", url)
        git(path, "config", "remote.origin.promisor", "true")
        git(path, "config", "remote.origin.partialclonefilter", "blob:none")
        git(path, "fetch", "--quiet", "--filter=blob:none", "--no-tags", "origin", ref + ":" + ref)
    return directory / "v", directory / "vc"


def github_outputs(manifest):
    validate_manifest(manifest)
    lines = [f"{field}={manifest[field]}" for field in ("tag", "v_source_sha", "vc_revision", "vc_c_sha256")]
    lines.append("manifest=" + json.dumps(manifest, sort_keys=True, separators=(",", ":")))
    return "\n".join(lines) + "\n"


def replace_exact(text, old, new, count, name):
    actual = text.count(old)
    if actual != count:
        raise PreparationError(f"{name}: expected {count} occurrences of {old!r}, found {actual}")
    return text.replace(old, new)


def read_sources(source):
    recipes = {}
    for name, expected in SOURCE_HASHES.items():
        path = source / "docker" / "vlang" / name
        try:
            data = path.read_bytes()
        except OSError as error:
            raise PreparationError(f"cannot read {path}: {error}") from error
        if hashlib.sha256(data).hexdigest() != expected:
            raise PreparationError(f"{name}: does not match vlang/docker@{DOCKER_REVISION}")
        recipes[name] = data.decode("utf-8")
    return recipes


def render_recipes(sources, manifest):
    validate_manifest(manifest)
    recipes = {name: text for name, text in sources.items() if name != "Dockerfile.nix.build"}
    for variant in ("alpine", "debian"):
        name = f"Dockerfile.{variant}.dev-full"
        recipes[name] = replace_exact(
            recipes[name],
            f"FROM @@USERN@@/vlang:{variant}-build\n",
            f"FROM ${{USER}}/vlang:{variant}-build\n",
            1,
            name,
        )

    toolchain = replace_exact(
        sources["Dockerfile.nix.build"],
        "FROM nixos/nix:latest\n",
        f"FROM nixos/nix:latest AS {TOOLCHAIN_STAGE}\n",
        1,
        "Nix toolchain stage",
    )
    toolchain = replace_exact(toolchain, "    make \\\n", "    gnumake \\\n", 1, "Nix package")
    toolchain = replace_exact(
        toolchain,
        "ENV PATH=/root/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/bin:/bin\n",
        f"ENV PATH={TOOLCHAIN_PATH}\n",
        1,
        "Nix compiler PATH",
    )
    for name, count in (("Dockerfile.nix", 2), ("Dockerfile.nix.dev-full", 1)):
        leaf = replace_exact(
            recipes[name],
            "FROM ${USER}/vlang:nix-build",
            f"FROM {TOOLCHAIN_STAGE}",
            count,
            name,
        )
        # The leaf's ARG declarations now belong to the toolchain stage. Its
        # child stages inherit them, including the requested V_REV build arg.
        recipes[name] = toolchain + "\n" + leaf

    for name, text in recipes.items():
        if re.search(r"@@[^\n]*@@", text):
            raise PreparationError(f"{name}: unresolved template placeholder")
        if re.search(r"^FROM\s+\S*/vlang:nix-build(?:\s|$)", text, re.MULTILINE):
            raise PreparationError(f"{name}: external Nix builder remains")
        if text.count('fetch --quiet --depth 1 origin "${V_REV}"') != 1:
            raise PreparationError(f"{name}: expected one V_REV fetch")
        if text.count("checkout --quiet FETCH_HEAD") != 1:
            raise PreparationError(f"{name}: expected one fetched-revision checkout")
        text = replace_exact(text, "RUN git clone --depth 1 https://github.com/vlang/v",
                             BOOTSTRAP_ARGS + "RUN git clone --depth 1 https://github.com/vlang/v", 1, name)
        text = replace_exact(text, "checkout --quiet FETCH_HEAD && \\\n",
                             "checkout --quiet FETCH_HEAD && \\\n" + BOOTSTRAP_INSTALL, 1, name)
        text = replace_exact(text, "    make ", "    make -o latest_vc ", 1, name)
        versions = re.findall(r"(?:\./)?v -version", text)
        if len(versions) != 1:
            raise PreparationError(f"{name}: expected one compiler version command")
        recipes[name] = replace_exact(text, versions[0], BOOTSTRAP_VERIFY + versions[0], 1, name)
    return recipes


def prepare(source, output, manifest, tag):
    validate_manifest(manifest, tag)
    recipes = render_recipes(read_sources(source), manifest)
    # Validate every input before creating any output; never modify the pinned
    # checkout or reuse recipes left by an earlier preparation.
    if output.exists():
        raise PreparationError(f"output already exists: {output}")
    output.mkdir(parents=True)
    for name, text in recipes.items():
        (output / name).write_bytes(text.encode("utf-8"))
    return recipes


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_mutually_exclusive_group(required=True)
    modes.add_argument("--source", type=Path, help="pinned vlang/docker checkout")
    modes.add_argument("--resolve-bootstrap", help="release or weekly tag whose bootstrap is resolved")
    parser.add_argument("--output", type=Path, help="new generated-recipe directory")
    parser.add_argument("--release-tag", help="tag that must match the bootstrap manifest")
    parser.add_argument("--bootstrap-manifest", type=Path, help="frozen resolver output")
    parser.add_argument("--history-dir", type=Path, help="new directory for isolated Git histories")
    parser.add_argument("--bootstrap-output", type=Path, help="new frozen manifest file")
    parser.add_argument("--github-output", type=Path, help="append validated outputs for the resolution job")
    args = parser.parse_args()
    try:
        if args.resolve_bootstrap is not None:
            if (args.history_dir is None or args.bootstrap_output is None or args.source is not None
                    or any(v is not None for v in (args.output, args.release_tag, args.bootstrap_manifest))):
                parser.error("resolver requires --history-dir/--bootstrap-output, without recipe arguments")
            if args.bootstrap_output.exists():
                raise PreparationError("bootstrap output already exists")
            v_repo, vc_repo = fetch_histories(args.resolve_bootstrap, args.history_dir)
            manifest = resolve_bootstrap(args.resolve_bootstrap, v_repo, vc_repo)
            encoded = json.dumps(manifest, indent=2) + "\n"
            with args.bootstrap_output.open("x", encoding="utf-8") as output:
                output.write(encoded)
            if args.github_output is not None:
                with args.github_output.open("a", encoding="utf-8") as output:
                    output.write(github_outputs(manifest))
            print(encoded, end="")
        else:
            if (args.output is None or args.release_tag is None or args.bootstrap_manifest is None
                    or any(v is not None for v in (args.history_dir, args.bootstrap_output, args.github_output))):
                parser.error("preparation requires --output/--release-tag/--bootstrap-manifest")
            manifest = read_manifest(args.bootstrap_manifest, args.release_tag)
            recipes = prepare(args.source, args.output, manifest, args.release_tag)
            print(f"Prepared {len(recipes)} release recipes from vlang/docker@{DOCKER_REVISION}")
    except (PreparationError, OSError, UnicodeError, subprocess.TimeoutExpired) as error:
        parser.exit(1, f"release recipe preparation failed: {error}\n")


if __name__ == "__main__":
    main()
