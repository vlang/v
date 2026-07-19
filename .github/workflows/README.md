# GitHub Actions CI policy

This directory contains the GitHub Actions workflows for the V repository.

V is tested on several operating systems, CPU architectures, containers, and C
compilers. Runner selection should therefore be intentional: use stable runner
images where reproducibility matters, and use newer runner images where the goal is
early compatibility feedback.

## CI lanes

V's CI should keep fast contributor feedback while preserving the broad portability
coverage needed for a compiler and runtime project. Prefer small, focused workflow
changes, and avoid removing coverage without a replacement lane.

Use these lanes when adding, moving, or splitting checks:

| Lane | Trigger | Blocking? | Purpose |
| --- | --- | --- | --- |
| Fast PR | every PR | yes | Quick build, formatting, docs, and critical smoke checks. |
| Standard PR | every PR or broad code paths | usually | Main Linux, macOS, and Windows compiler coverage. |
| Specialized PR | relevant paths | maybe | Domain checks such as docs, db, graphics, wasm, C2V, or VPM. |
| Slow PR | label, manual, or high-risk paths | usually no | Expensive portability and deep regression checks. |
| Master push | every merge to `master` | monitoring | Full confidence after merge and early regression detection. |
| Scheduled | cron | no | Nightly or periodic portability, ecosystem, and performance sweeps. |

When classifying a workflow:

- Keep every PR covered by a small fast lane with clear failure signal.
- Keep required branch protection checks stable unless maintainers intentionally change
  the required check list at the same time.
- Prefer `paths` for workflows that only validate a subsystem, and include the workflow
  file itself in the path list so CI changes are self-tested.
- Keep `workflow_dispatch` on slow or rare-platform workflows so maintainers can rerun
  them directly.
- Keep broad post-merge coverage on `master` before moving expensive PR checks to path,
  label, manual, or scheduled execution.
- Document exceptions in the workflow when a slow check must block every PR.

Current slow-lane candidates include FreeBSD, OpenBSD, Termux, `riscv64`, `s390x`,
the full sanitizer matrix, and full ecosystem compilation workflows. These checks are
valuable; the policy is to run them where they provide useful signal, not to remove them.

## Runner policy

### General guidance

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

### Linux runners

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

### Windows runners

Windows runner images include different SDKs, toolchains, and bundled compiler
versions. Use an explicit Windows version when a workflow depends on a known MSVC,
MinGW, Windows SDK, or bundled tool layout.

Use different Windows versions only when the coverage difference is intentional. If two
similar workflows use different Windows runner images, document why in the workflow or
in this README.

### macOS runners

macOS runner names can imply both OS version and CPU architecture. For example, some
workflows intentionally use Apple Silicon runners while others need Intel runners.

Use explicit macOS runner names when architecture matters, especially for release
artifacts, prebuilt binaries, GUI checks, cross-compilation checks, or platform-specific
toolchain behavior.

Avoid switching macOS runners in broad CI changes unless the PR explains the expected
architecture and toolchain impact.

### Rare platform and architecture lanes

Workflows for FreeBSD, OpenBSD, Termux, `riscv64`, `s390x`, and similar lanes often
depend on hosted runner behavior plus containers, VMs, or emulation layers. Treat their
runner image as part of the tested environment.

When changing these workflows, prefer small PRs and keep `workflow_dispatch` available
so maintainers can rerun the lane directly.

### Updating runner images

Before updating a runner image:

1. Check whether the workflow has comments explaining the current runner.
2. Check related workflows for intentionally different runner versions.
3. Review GitHub's runner image changelog for toolchain, SDK, and package changes.
4. Keep the PR focused on the runner update when possible.
5. Explain the expected impact in the PR body.

If a runner update is needed because GitHub deprecated an image, mention the deprecation
notice or upstream issue in the PR body.

### Current examples

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

## Apt package cache profiles

Use `.github/actions/cache-apt-packages-action` with the smallest profile that covers
the packages a job installs. Smaller profiles reduce cache restore time, cache size,
and accidental coupling between unrelated workflows.

Available profiles:

| Profile | Use for |
| --- | --- |
| `common` | General compiler/tooling jobs that need common command-line packages. |
| `docs` | Markdown and documentation checks that compile examples using docs deps. |
| `db` | Jobs that need PostgreSQL, SQLite, or database development headers. |
| `graphics` | Jobs that compile or run graphical, X11, OpenGL, or font-related examples. |
| `sdl` | SDL-based workflows such as games or multimedia examples. |
| `sanitizers` | Sanitizer jobs that need compiler, valgrind, db, and graphics deps. |
| `full` | Compatibility default for jobs not migrated to a narrower profile yet. |

When migrating a workflow, keep the job's explicit install step until CI proves the
profile is sufficient. The cache action speeds up installs; it should not hide which
packages the workflow actually needs.
