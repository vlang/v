# GitHub Actions CI policy

This directory contains the GitHub Actions workflows for the V repository.

V is tested on several operating systems, CPU architectures, containers, and C
compilers. Runner selection should therefore be intentional: use stable runner
images where reproducibility matters, and use newer runner images where the goal is
early compatibility feedback.


## Master push gate

All master-push CI is orchestrated by `ci_master.yml`. It starts Linux,
macOS, Windows and Termux bootstrap groups in parallel, alongside the
Linux/Windows `makev_ci.yml` bootstrap contracts. Every bootstrap producer runs
`test-cleancode` and `test-fmt` in order with its optimized compiler before
uploading the artifact, including both Termux containers. Once **Linux** bootstrap
succeeds, the `preflight` job checks CI helpers and selects suites, then
`gen_vc_ci.yml` generates and checks `v.c` and `v_win.c` on every
master push, including documentation-only changes and empty diffs. Linux
suites can start after these checks without waiting for macOS or Windows
bootstrap. macOS, Windows and Termux suites that only target those platforms
also require their respective bootstrap group. Windows suites require the
Windows bootstrap contracts as well. Mixed-platform suites may start with the
Linux gate and use a source-built compiler if their platform's artifact is
not available yet. All suites still require successful VC generation.

Each failed bootstrap group still fails the overall CI run, but does not
cancel unrelated platforms' tests. This preserves Linux results even if a
macOS bootstrap runner is slow or fails. Compiler artifact cleanup waits for
**all** bootstrap groups and Windows contracts to succeed before pruning
older artifacts.

Existing suites are called as reusable workflows via `workflow_call`, with
eight small, formerly standalone checks inlined in the caller to respect
GitHub Actions' 50-unique-reusable-workflow limit. Original ordered path
filters live in `ci_gate_filters/*.json` (with VC generation set to `all`) and are applied by
`ci_gate_paths.py` to the exact before/after SHAs from the push. An
unavailable diff conservatively runs all suites instead of skipping them.
The gate validator rejects newly added direct-push workflows that would
bypass this dependency.

When adding or changing a master-push suite, update the caller and path
manifest together. Scheduled, manually dispatched and pull-request workflows
retain their independent existing triggers; their non-push invocations are
not part of this master-push gate.

## Optimized test compilers

Ordinary test jobs use `.github/actions/setup-v` to install a compiler built with
`v self -prod`. This optimizes the compiler itself. Each job's `VFLAGS`, C compiler,
sanitizer flags and debug/release test variants still control the programs under test.

The bootstrap workflow produces and checks compilers for Linux x64, macOS ARM64,
macOS x64, Windows x64, Termux ARM64 and Termux x64. Linux is built on Ubuntu 22.04 so
the binary also runs on the newer glibc runners. Termux uses Clang inside the official
`termux/termux-docker` images on matching native ARM64 and x64 runners. Each Termux
producer runs a sample program and rebuilds the compiler before uploading its binary.
Each platform-specific suite waits for its producer, while Linux suites
need only the Linux producer. Mixed-platform suites may fall back to a
locally optimized compiler when an artifact has not yet been produced.
A failing producer still marks the full master CI run as failed.

Each producer uploads a seven-day artifact named `vprod-<platform>-<full-commit-sha>`.
It contains `<full-commit-sha>-<platform>` and its `.sha256` checksum. The setup action
uses these artifacts within the same master workflow run and verifies their checksum,
checkout commit and platform before installing them as `v` or `v.exe`. Rerunning a
producer replaces its workflow artifact. The binaries are available from the `vlang/v`
Actions run page, outside GitHub Releases, and require no additional token or secret.

Shared compilers resolve their standard library from the checkout containing the installed
executable. This supports nested `working-directory` checkouts and compilation from external
project directories. A standalone compiler outside a checkout retains its build-time source
root as a fallback.

After all platform bootstraps, Windows bootstrap contracts, preflight checks,
and VC generation succeed, a separate job removes older artifacts from
completed `CI master` push runs. It preserves artifacts belonging to the current run,
newer runs and runs still in progress, and only deletes names matching the owning run's
exact commit and one of the six producer platforms. Other test artifacts are unaffected.
An older commit's rerun cannot remove newer binaries. Cleanup uses the automatic
`GITHUB_TOKEN` with `actions: write` only in that job; test jobs keep their existing
permissions. Cleanup failure does not block tests, and seven-day expiry bounds retention
when no later successful master gate performs cleanup.

Standalone jobs and reruns whose artifacts were removed build an optimized compiler
locally. The setup action also supports `use-prebuilt: false` for jobs that require a native
build. Shell-only jobs can call `prepare_v.sh`; set `V_CI_SOURCE_BUILD=1` to ignore a supplied
prebuilt compiler and optionally `V_CI_CC` to choose the optimizing C compiler.
`prepare_v.ps1` provides the corresponding Windows build/install step.

Termux artifacts use the platform names `termux-arm64` and `termux-x64`. Install their binary
as `v` in a checkout of the matching commit after verifying its `.sha256` checksum, and restore
its executable permission with `chmod +x v`. The normal Termux dependencies (`clang`,
`libexecinfo`, `libgc` and `libgc-static`) are still required. These binaries target Android's
Bionic libc and are separate from the glibc Linux compiler. The setup helper detects Termux
before considering Linux artifacts and only reuses them for a verified 64-bit userland.
The dedicated Termux test workflow still builds from source.

Alpine/musl, non-Termux Android, BSD and other CPU architectures build natively. Bootstrap, update,
prebuilt-release and compiler-under-test checks retain their specific build recipes;
ordinary tests use the optimized driver. Instrumented compiler builds retain their sanitizer
flags and add `-prod`. Compiler self-build checks use separate executables or restore the
optimized driver afterward. Setup still provisions TCC, libgc and bootstrap sources.

Compiler CI also sets `V3_TEST_PROD_COMPILER=1` so `test_all.vsh` optimizes the separate
V3 drivers it creates for unit tests and examples. Plain nested compiler builds share that
optimized driver; requests with custom build options still build their own compiler. The unit
job builds it once before starting its shards, which each receive a private copy. Local runs
can select another optimizing C compiler with `V3_TEST_PROD_COMPILER_CC`.

Changes to shared compiler setup or the bootstrap workflow select all master suites, even
when a suite's normal path filter would skip them. Local setup regression checks are
`python3 .github/workflows/prepare_v_test.py` and `python3 cmd/tools/windows_ci_test.py`;
artifact cleanup checks are `python3 .github/workflows/cleanup_vprod_test.py`.

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

The main macOS CI matrix uses `macos-15` and `macos-26` Apple Silicon runners.
CI lanes previously pinned to `macos-14` now use `macos-15` to retain Apple Silicon
coverage on a supported image. Prebuilt smoke tests select `macos-15` for ARM64 and
`macos-15-intel` for x86_64 so each release archive runs on its native architecture.
GitHub retired macOS 14 images on November 2, 2026; see the
[retirement announcement](https://github.com/actions/runner-images/issues/13518).

### Rare platform and architecture lanes

Workflows for FreeBSD, OpenBSD, Termux, `riscv64`, `s390x`, and similar lanes often
depend on hosted runner behavior plus containers, VMs, or emulation layers. Treat their
runner image as part of the tested environment.

FreeBSD CI example builds use one worker (`VJOBS=1`) to leave memory for the CI script's
compiler processes in the 8 GiB VM. The complete example selection is retained;
the other FreeBSD test tasks keep their normal worker counts.

Portable compiler snapshots keep signal masks in aligned opaque storage, so the
OpenBSD bootstrap does not need the native SDK's `sigset_t` typedef.

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
| `macos-15`, `macos-26` | Apple Silicon macOS CI lanes. |
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
- `prebuilt.yml` uses `windows-latest` to smoke-test the published Windows release
  ZIP against GitHub's current hosted Windows image. Its macOS jobs pin the runner
  architecture to match each release ZIP.

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

MemorySanitizer jobs use `ubuntu-24.04` and its default Clang 18 toolchain for
instrumented V compilers, instrumented tests, and the private database libraries.
Ubuntu 22.04's Clang 14 runtime
lacks the `stat64`/`lstat64` interceptors needed by SQLite's large-file pathname
checks. Even an instrumented SQLite library then sees stale shadow memory after
a successful `lstat64` call when opening a database on disk. Clang 18 supplies
these interceptors, preserving the normal disk-path and symbolic-link checks.
The AddressSanitizer and UndefinedBehaviorSanitizer jobs stay on `ubuntu-22.04`.
MemorySanitizer uses the explicit dependency install rather than the shared
sanitizer apt cache, whose key does not distinguish Ubuntu versions.

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
