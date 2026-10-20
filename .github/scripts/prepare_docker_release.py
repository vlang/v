#!/usr/bin/env python3
"""Prepare release recipes from the reviewed vlang/docker snapshot.

This deliberately handles one pinned recipe version, rather than rewriting
arbitrary Dockerfiles. A companion update must review these hashes and edits.
"""

import argparse
import hashlib
from pathlib import Path
import re


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


class PreparationError(ValueError):
    """The pinned input or an expected replacement does not match."""


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


def render_recipes(sources):
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
    return recipes


def prepare(source, output):
    recipes = render_recipes(read_sources(source))
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
    parser.add_argument("--source", required=True, type=Path, help="pinned vlang/docker checkout")
    parser.add_argument("--output", required=True, type=Path, help="new generated-recipe directory")
    args = parser.parse_args()
    try:
        recipes = prepare(args.source, args.output)
    except (PreparationError, OSError) as error:
        parser.exit(1, f"release recipe preparation failed: {error}\n")
    print(f"Prepared {len(recipes)} release recipes from vlang/docker@{DOCKER_REVISION}")


if __name__ == "__main__":
    main()
