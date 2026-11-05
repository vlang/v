# GitHub Actions CI policy

This directory contains the GitHub Actions workflows for the V repository.

V is tested on several operating systems, CPU architectures, containers, and C
compilers. Runner selection should therefore be intentional: use stable runner
images where reproducibility matters, and use newer runner images where the goal is
early compatibility feedback.

## CI lanes

CI runs only for commits on `master`. Pull requests, merge queues, tags, and pushes
to other branches do not trigger CI. Existing path filters still select the checks
needed for each `master` push.

Manual and scheduled workflows keep a job condition requiring
`github.ref == 'refs/heads/master'`. Select `master` when dispatching a workflow;
jobs are skipped for any other ref. Workflow completion triggers must also filter
their upstream runs to `master`. Reusable workflow triggers are disabled so callers
cannot start these checks from a pull request or another branch.

Use these lanes when adding, moving, or splitting checks:

| Lane | Trigger | Blocking? | Purpose |
| --- | --- | --- | --- |
| Master push | push to `master` | monitoring | Compiler, formatting, docs, and portability coverage. |
| Specialized | relevant paths on `master` | monitoring | Domain checks such as db, graphics, wasm, or VPM. |
| Scheduled | cron on `master` | no | Periodic portability, ecosystem, and performance sweeps. |
| Manual | dispatch from `master` | no | Focused reruns, release publishing, and recovery. |

When classifying a workflow:

- Add only `master` to push branch filters; do not add pull request or merge queue triggers.
- Guard every job in manual or scheduled workflows with the `master` ref condition,
  including jobs using `always()`.
- Prefer `paths` for workflows that only validate a subsystem, and include the workflow
  file itself in the path list so CI changes are self-tested.
- Keep `workflow_dispatch` on slow or rare-platform workflows so maintainers can rerun
  them directly from `master`.
- Keep broad coverage on `master`, including slow portability and ecosystem checks.

The issue voting information workflow is administrative automation and retains its
`issues` trigger.

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

The Win32 W3, W4, and W5 runners pin exact test inputs and runner hashes. When changing
those inputs, update the ordered manifests, composite hashes, and all three Windows
workflow callers together. Keep backend-state assertions in V so test helpers use
the compiler's current symbol names and integer types. Test C headers must include
fallback definitions for Windows messages missing from TinyCC's bundled headers.
The bootstrap contract checks W5 source hashes, ordered surfaces, test selection,
and all three compiler callers so formatting or type migrations cannot leave stale pins.

Windows compiler jobs create a private tool cache with the runner user as its owner.
The cache uses a short path on the runner's temporary drive so MSVC's nested helper
object paths stay below `MAX_PATH`. `VTMP` uses the same short root so test sessions
and their isolated module caches also fit within the native tools' path limits.
The main jobs verify that a second `v vet` invocation reuses its compiled tool before
starting `test-cleancode`. A cache rejected because of directory ownership or permissions
would otherwise rebuild the tool for every input until the job times out.

Pkg-config steps run the current compiler's argument-query and compile-time detection
tests. The bootstrap contract also checks that explicit source and test paths in the
Windows workflows exist; archived V1 tests under `vlib/v/legacy_tests` are fixtures.
Direct C compilation checks link V3's native runtime helpers, disable GC and FastC
dependencies, and run the resulting compiler's version command.
Cross-platform C-generation probes select a compiler for their target explicitly;
the Windows job's `VFLAGS` must not select MSVC for macOS output.
Compatibility-compiler installation gives checksum tools a valid null stdin, including
when the parent compiler launches the installer without an input handle. The bootstrap
contract exercises that closed-stdin case.
D3D11 shader fixtures own their SM4 bytecode rather than depending on definitions
from the retired `sokol_gl` renderer. The bootstrap contract validates their DXBC
containers without including that renderer.
The TCC tools step also runs `oldv` shell-command regressions, including native
Windows quoting, variable expansion, loop syntax, and nonzero exit propagation.
Pure V math steps use the `pure_v_math` define and a glob with Windows path
separators. The glob excludes C overrides; the define selects the portable math
test expectations and keeps that build separate from normal C math caches.

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
with the `master` job condition so maintainers can rerun the lane directly.

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
| `windows-11-arm` | Native ARM64 Windows release artifact lane. |
| `macos-14` | Apple Silicon CI lanes. |
| `macos-15-intel` | Intel macOS release artifact lane. |
| `*-latest` | Small utility, smoke, or intentionally floating lanes. |

Current release and artifact exceptions that intentionally use floating runners:

- `release_ci.yml` uses `windows-latest` for `v_windows.zip` so the published
  Windows artifact tracks GitHub's current hosted Windows toolchain.
- Its `windows-11-arm` lane builds `v_windows_arm64.zip` with native MSVC tools using
  `cmd/tools/builders/windows_arm64_release.ps1`. The script checks PE architecture
  for the x64 bootstrap and the native ARM64 compiler stages, application smoke test,
  and release tools. The transient x64 bootstrap runs under Windows 11's emulation;
  all shipped binaries are ARM64.
  The ARM64 package uses `-cc msvc` and does not contain the x64 TCC bundle.
  Tags that predate the ARM64 bootstrap script skip this lane and its archive.
- `release_ci.yml` uses `ubuntu-latest` only for the release publishing job; the
  job assembles already-built artifacts and does not compile platform binaries.
- `prebuilt.yml` uses `macos-latest` and `windows-latest` to smoke-test the
  published release ZIPs against GitHub's current hosted macOS and Windows images.

## Ecosystem module cache

`make` warms separate core module cache entries for native programs and tests, using
the same compiler and GC flags for both. Keep its C compiler configuration aligned
with the later builds: the macOS GUI jobs select `-cc clang`
before `make`, matching the GUI script. The other app and VPM jobs retain their
default compiler selection. Explicit compiler, debug, and production options use
their own cache configurations, as do different C flags and interface implementations.

`should-compile-all` uses the default C backend for ordinary native examples so they
can link the cached modules. It still selects JS and Wasm for their filename suffixes
and overrides an inherited backend when selecting C examples. An explicit `-b c`
requests a complete C translation unit and bypasses the module cache.

## Apt package cache profiles

Use `.github/actions/cache-apt-packages-action` with the smallest profile that covers
the packages a job installs. Smaller profiles reduce cache restore time, cache size,
and accidental coupling between unrelated workflows.

Available profiles:

| Profile | Use for |
| --- | --- |
| `common` | General compiler/tooling jobs that need common command-line packages. |
| `docs` | Markdown checks that compile examples using docs, db, SSL, X11, or graphics deps. |
| `db` | Jobs that need PostgreSQL, SQLite, or database development headers. |
| `graphics` | Jobs that compile or run graphical, X11, OpenGL, or font-related examples. |
| `sdl` | SDL-based workflows such as games or multimedia examples. |
| `sanitizers` | Sanitizer jobs that need compiler, valgrind, db, and graphics deps. |
| `full` | Compatibility default for jobs not migrated to a narrower profile yet. |

When migrating a workflow, keep the job's explicit install step until CI proves the
profile is sufficient. The cache action speeds up installs; it should not hide which
packages the workflow actually needs.

## MemorySanitizer dependencies

The `memory-clang` self-test job builds a pinned libpq with MemorySanitizer, including its
bundled support objects. The distribution library is not instrumented and can leave sanitizer
shadow memory stale when it constructs buffers inside `PQconnectdb`. The job selects the private
build through `PKG_CONFIG_PATH` and `LD_LIBRARY_PATH`; PostgreSQL tests keep their normal
assertions.
The other sanitizer jobs continue using the distribution library. OpenSSL remains a system
dependency, so this does not claim that every external library has MemorySanitizer coverage.

## Release upgrade checks

The upgrade checks in `vup_works.yml` extract release ZIPs with `ditto` on macOS and
`unzip` on Linux. The macOS archives contain symbolic links for their bundled libraries.
All four upgrade scenarios start with a fresh extraction and stop if extraction fails
or the released compiler is missing or not executable.

## Rebuilding release binaries

`Release CI` builds without publishing when its workflow file changes on `master`.
To publish ZIPs for an existing `weekly.*` or `0.*` tag, dispatch the workflow from `master`
and set `release_tag`, for example `weekly.2026.41`. It checks out and verifies that tag,
builds the five original platform archives and, when supported by the tag, Windows ARM64,
and attaches them to the corresponding release. Existing
assets with the same filenames are replaced; release descriptions, names, and prerelease status
are kept. The publishing job runs only after every platform build succeeds, and upload errors
fail it. Tag pushes do not start workflows.

Leaving `release_tag` empty keeps manual runs in build-only mode. A branch named like a
release cannot substitute for the requested tag.

```sh
gh workflow run release_ci.yml --ref master -f release_tag=weekly.2026.41
```

Docker release publishing and prebuilt binary smoke tests are also manual workflows
dispatched from `master`:

```sh
gh workflow run hub_docker_ci.yml --ref master -f tag=weekly.2026.41
gh workflow run prebuilt.yml --ref master
```
