# GitHub Actions runner policy

This directory contains the GitHub Actions workflows for the V repository.

V is tested on several operating systems, CPU architectures, containers, and C
compilers. Runner selection should therefore be intentional: use stable runner
images where reproducibility matters, and use newer runner images where the goal is
early compatibility feedback.

## General guidance

- Prefer explicit runner versions for important CI lanes.
- Use `ubuntu-latest`, `windows-latest`, or `macos-latest` only when image drift is
  acceptable for that workflow.
- Prefer conservative, reproducible runner choices for release and artifact workflows.
  When a release or artifact workflow intentionally uses `*-latest`, document why.
- Keep platform-specific workflows on the runner version that best matches the
  platform risk being tested.
- Mention the reason in the workflow when a runner version is intentionally older,
  newer, or different from similar workflows.
- When changing a runner image, check whether the workflow depends on bundled tools,
  SDKs, compiler versions, package repositories, or architecture-specific behavior.

## Linux runners

Use a pinned Ubuntu version when the workflow depends on packages, compiler versions,
or distro behavior. This makes failures easier to reproduce and avoids unexpected
changes when GitHub updates `ubuntu-latest`.

Use `ubuntu-latest` for small utility workflows where image drift is low risk, or when
the workflow is intended to track GitHub's current default Linux image.

Examples of reasons to pin Linux runners:

- the workflow installs distro packages with `apt`;
- the workflow validates a specific distro or compiler version;
- the workflow runs release or artifact generation;
- the workflow uses architecture emulation or containers that are sensitive to host
  image changes.

## Windows runners

Windows runner images include different SDKs, toolchains, and bundled compiler
versions. Use an explicit Windows version when a workflow depends on a known MSVC,
MinGW, Windows SDK, or bundled tool layout.

Use different Windows versions only when the coverage difference is intentional. If two
similar workflows use different Windows runner images, document why in the workflow or
in this README.

## macOS runners

macOS runner names can imply both OS version and CPU architecture. For example, some
workflows intentionally use Apple Silicon runners while others need Intel runners.

Use explicit macOS runner names when architecture matters, especially for release
artifacts, prebuilt binaries, GUI checks, cross-compilation checks, or platform-specific
toolchain behavior.

Avoid switching macOS runners in broad CI changes unless the PR explains the expected
architecture and toolchain impact.

## Rare platform and architecture lanes

Workflows for FreeBSD, OpenBSD, Termux, `riscv64`, `s390x`, and similar lanes often
depend on hosted runner behavior plus containers, VMs, or emulation layers. Treat their
runner image as part of the tested environment.

When changing these workflows, prefer small PRs and keep `workflow_dispatch` available
so maintainers can rerun the lane directly.

## Updating runner images

Before updating a runner image:

1. Check whether the workflow has comments explaining the current runner.
2. Check related workflows for intentionally different runner versions.
3. Review GitHub's runner image changelog for toolchain, SDK, and package changes.
4. Keep the PR focused on the runner update when possible.
5. Explain the expected impact in the PR body.

If a runner update is needed because GitHub deprecated an image, mention the deprecation
notice or upstream issue in the PR body.

## Current examples

These examples document observed runner choices. They are not a complete workflow
inventory.

| Runner choice | Example use |
| --- | --- |
| `ubuntu-24.04` | Main Linux and container-oriented CI lanes. |
| `ubuntu-22.04` | Some sanitizer, release, and architecture lanes. |
| `windows-2025` | GCC Windows lane and selected Windows checks. |
| `windows-2022` | MSVC and TCC Windows lanes. |
| `macos-14` | Apple Silicon CI lanes. |
| `macos-15-intel` | Intel macOS release artifact lane. |
| `*-latest` | Small utility, smoke, or intentionally floating lanes. |

Current release and artifact exceptions that intentionally use floating runners:

- `release_ci.yml` uses `windows-latest` for `v_windows.zip` so the published
  Windows artifact tracks GitHub's current hosted Windows toolchain.
- `release_ci.yml` uses `ubuntu-latest` only for the release publishing job; the
  job assembles already-built artifacts and does not compile platform binaries.
- `prebuilt.yml` uses `macos-latest` and `windows-latest` to smoke-test the
  published release ZIPs against GitHub's current hosted macOS and Windows images.
