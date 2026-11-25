# Linux freestanding C ABI regressions

These fixtures exercise the native C adapters in `builtin/linux_bare` through their public
C declarations. The normal regression links the actual bare allocator, TLS, startup,
syscalls, and thread runtime. Run it from the repository root:

```sh
env -u VEXE -u VFLAGS ./v -silent vlib/v/compiler_tests/freestanding_compat_test.v
```

The regression requires Linux amd64 or arm64, GCC, Clang, Python 3, and local socket access.
It checks 16 combinations of C compiler, production/debug mode, `-d vlibc`, and
`-target-libc-headers`. These explicitly static fixture ELFs are checked for absent dynamic
and interpreter segments. The flags must not contain a hosted pthread library dependency.

The ABI probe checks file metadata and sparse offsets, descriptor errors, IPv4/IPv6,
socket pairs, TCP accept flags, sendfile, epoll/eventfd, signals and masks, allocation,
calendar conversion, and affinity. Stdio checks include buffering, append and update streams,
read-ahead accounting, line buffering, EOF/errors, delayed write errors, and process-exit flushes.
Mutex checks cover thread contention, recursive and error-checking ownership, deadlines,
attributes, and a counter shared with a separate clone process.

The formatter oracle compares fixed binary records against the host C library for GCC and
Clang in debug and production builds. It covers truncation and guard bytes, integer lengths,
positional and wide conversions, `%n`, floating-point boundaries, long doubles, and 10,000
deterministic double patterns across all four rounding modes. Separate contracts test huge
precision and overflow without reproducing host-specific behavior outside the C contract.

The DNS fixture supplies isolated hosts, services, and resolver files and starts local UDP/TCP
servers on an ephemeral port. Cases cover search domains, IPv4/IPv6 and mapped addresses,
canonical aliases, TCP fallback with fragmented frames, malformed compression, mismatched
questions/IDs, NXDOMAIN, server failures, and timeouts. Allocation failures are injected at
successive allocation points; descriptor exhaustion is injected separately. Each failure must
release owned memory and descriptors. Another probe submits 50,000 deterministic malformed
packets placed next to an inaccessible guard page and rejects partial results on failure.
Reverse lookup cases exercise numeric IPv4/IPv6 and scopes, Unix sockets, hosts/services,
PTR and CNAME chains, fragmented TCP retry, malformed records, alias/compression cycles,
NXDOMAIN, server failure, descriptor failure and bounded output. The server records PTR
queries and requires the truncated-response case to reach its TCP listener.

The native runtime oracle also checks `glob`/`glob64` brace order, escaping, hidden files,
symlinked directories, dangling links, directory marking, home expansion, native callbacks,
callback errors and repeated cleanup. Append tests retain caller-owned reserved slots and
previous ordering. Sparse-file checks use offsets above 4 GiB; error-text checks cover
GNU/POSIX return values, truncation guards, unknown errors and unchanged errno.
Cancellation checks compare the hosted runtime with native debug/production builds for blocked
read/recv/poll, pending requests across disable/enable, asynchronous interruption, explicit exit,
condition mutex ownership, canceled joiner recovery, nested cleanup order and TLS destructors.
The native C++ fixture separately checks RAII and catch/rethrow behavior through C++ cleanup
classes, C jump-buffer cleanup frames, asynchronous signals and explicit `pthread_exit`.
Thread checks also compare stack bounds, fork callback ordering, and shared semaphores.
Thread metadata cases check self/foreign names, Unicode/newline/empty names, size guards,
scheduling parameters, priority ranges, affinity masks and errno preservation. A fork child
creates another worker that renames its surviving main thread. Ordinary niceness increments
compare against the host; the native-only `INT_MAX` check also verifies overflow-safe clamping.

For additional memory and undefined-behavior checks:

```sh
python3 vlib/builtin/testdata/freestanding_compat/sanitizers.py /tmp/bare-asan clang
```

The harness uses this checkout's `./v` to generate the V runtime and obtain its C adapter list.
It instruments both with ASan and UBSan, then renames symbols in the compiled objects so the
test can call the shim alongside the sanitizer's host libc. The V runtime and C adapters
share ASan's allocator for buffers they exchange. The host creates the test threads, and
leak detection stays enabled. The freestanding compatibility matrix separately exercises
the production heap and thread startup. The sanitizer run also compares every byte of the
formatter output with a system-libc executable and runs the local DNS fixture.

Cross-target checks can use an AArch64 Linux C compiler with native target headers and its
compiler arithmetic runtime, followed by `qemu-aarch64`. Zig's C driver is one possible test
toolchain; no Zig sources or Zig build dependency are added to V. Build `main.c.v`,
`oracle.c.v`, and `resolver.c.v` with `-os linux -arch arm64 -freestanding`. For a formatter
comparison between different architectures, compile both oracles with
`-cflags -DV_FORMAT_ORACLE_NO_EXTENDED=1` to exclude differing `long double` representations.
The DNS runner accepts a command prefix, for example:

```sh
python3 vlib/builtin/testdata/freestanding_compat/dns_fixture.py /tmp/bare-arm-dns \
  qemu-aarch64 /tmp/bare-arm-resolver
```

The font-stack probe additionally checks legacy and C23 binary-prefix integer parsing,
scanf assignment and allocation, floating-point boundaries, growing printf output, sorting,
C/POSIX and UTF-8 locales, incremental decoding errors, locale restoration, ISO week dates,
nonlocal jumps with signal-mask restoration, exit callbacks, entropy, and BSD random sequences.
Filesystem checks exercise directory iteration, symlinks and canonical paths, vector I/O,
mappings, select, ioctl, passwd lookup, and C/V environment interoperability. Eight V workers
exercise condition-variable broadcast and timeout behavior. A child-process regression checks
that stdout and stderr readiness use Linux's ioctl request before reading pending output.

An additional hosted oracle checks integer parsing in bases 2 through 36, 100,000 deterministic
floating-point strings, all four rounding modes for float/double/long double, UTF-8 conversion,
and each BSD random-state size:

```sh
python3 vlib/builtin/testdata/freestanding_compat/numeric_sanitizers.py /tmp/numeric-asan clang
```

Both sanitizer scripts also accept `gcc` as their last argument.

## Optional real X11 stack

The optional dependency fixture requires Python 3.12 or newer, curl, make, pkg-config,
autotools-generated release sources, Meson, Ninja, CMake, and the Xorg protocol development
headers (`xorgproto` and `xcb-proto`). It builds pinned releases of X11, Xft, fontconfig, xcb,
Xrender, Xau, Xdmcp, FreeType, Expat, PNG, zlib, bzip2, Brotli and OpenLibm into an isolated
prefix. OpenLibm supplies `-lm` for applications that import V's regular math module.
It installs nothing into system directories. Download hashes, build logs, and a resumable build
state remain in the chosen work directory. Built-in X11 internationalization stays enabled;
loadable i18n/Xcursor plugins are disabled. FreeType retains PNG, zlib, bzip2 and Brotli support;
HarfBuzz integration is disabled to avoid introducing a separate C++ dependency stack.
Builds use one compiler worker by default. `--jobs N` changes the total compiler-worker limit;
independent libraries do not start additional build pools.

```sh
python3 vlib/builtin/testdata/freestanding_compat/build_x11.py /tmp/vlibc-x11
./v -new-compiler -no-retry-compilation -syscalls -d vlibc -no-system-libc -cc gcc \
  -ldflags "-static @/tmp/vlibc-x11/link.rsp" -o /tmp/bar /path/to/bar
```

To install the dependencies persistently for ordinary V commands:

```sh
python3 vlib/builtin/testdata/freestanding_compat/build_x11.py /tmp/vlibc-x11 --install
./v -syscalls -vlibc -no-system-libc -ldflags -static -cc gcc -o /tmp/bar /path/to/bar
```

The installer publishes a versioned prefix beneath `~/.vmodules/vlibc/.store` and atomically
registers `~/.vmodules/vlibc/linux-<arch>-static`. It does not replace unmanaged files or
directories.
Registered archives, headers, font configuration, and pkg-config files survive deletion of the
build work directory. `VMODULES` changes the module root; `--prefix /path` installs an explicit
prefix that can be selected through `VLIBC_PREFIX=/path`. Explicit and automatically selected
prefixes are target-checked before use. The compiler does not download or build missing libraries.

The dependency regression builds an archive cycle with a `Libs.private` dependency in a prefix
containing spaces. Its GCC/Clang builds use explicit
`-syscalls -vlibc -no-system-libc -ldflags -static` commands,
check that all
libraries reach the link, execute the result, and reject dynamic/interpreter ELF segments.
It also checks that a missing archive reports its original linker error without a legacy retry.

```sh
./v -silent vlib/v/compiler_tests/freestanding_native_dependencies_test.v
```

The response file supplies archive groups for cyclic native dependencies. Existing module
`#flag -lX11`, `-lXft`, and `-lfontconfig` directives continue to work with that prefix.
The resulting program uses the prefix's fontconfig configuration by default. To use an existing
system font configuration instead, set `FONTCONFIG_FILE=/etc/fonts/fonts.conf` when running it.
Fonts and configuration files are runtime data; they are not embedded in the executable.

For an explicitly static application, check `readelf -l`, `readelf -d`, and `nm -u`: the result
must have no interpreter, dynamic dependencies, or unresolved symbols. Use an isolated Xvfb server
for GUI integration checks; a successful static link alone does not validate rendering or input.

## Shared libraries

To build the same complete library APIs as libc-free shared libraries:

```sh
python3 vlib/builtin/testdata/freestanding_compat/build_x11.py \
  /tmp/vlibc-x11-shared --install --shared
./v -syscalls -vlibc -no-system-libc -cc gcc -o /tmp/bar /path/to/bar
```

This registers the separate `linux-<arch>-shared` native prefix. The shared libraries contain
all archive members, use position-independent code, and link with `-nostdlib`. Their dependency
edges refer to the other libraries in that installation; `$ORIGIN` resolves those edges at runtime.
Distinct `-vlibc.so` SONAMEs prevent substitution by host libc-dependent libraries. Public
`-lX11`, `-lXft`, `-lfontconfig` and `-lm` resolve through aliases in the selected prefix.
The compiler adds the selected installation's runtime and link-time dependency search paths.
No system library is rewritten, and no automatic `-static` or static pkg-config query is added.

The dynamic regression loads two libc-free shared libraries with initialized, zero-filled,
and over-aligned TLS. It checks C/V heap interoperability, the constructor environment, ordinary
and detached V workers, process maps without libc, and one destructor call on return and `exit`.
GCC and Clang run the fixture in debug and production modes with `-vlibc -no-system-libc`,
its define alias, `-shim-libc`, and `-freestanding`.

```sh
./v -silent vlib/v/compiler_tests/freestanding_dynamic_test.v
```

The C process regression exercises `vfork`, session creation, descriptor duplication, and
`execve` across GCC/Clang debug and production builds with static and dynamic linking.
It checks parent-stack preservation, failed execution, argv/environment, and descriptor flags:

```sh
./v -silent vlib/v/compiler_tests/freestanding_process_test.v
```

The subprocess capture regression covers literal arguments, live environment changes, merged
output larger than a pipe buffer, inherited stdin, exit/signal status, execution failures,
executable scripts, shell pipelines and concurrent captures. It checks hosted,
syscall and vlibc builds, including production and autofree modes. Production checks remain active:

```sh
./v -silent vlib/v/compiler_tests/freestanding_capture_test.v
```

### Native runtime implementation regression

`vpac_runtime.c.v` calls the replacement runtime through native C declarations. It covers
thread creation/join/exit, key destructors, once initialization, read/write locks, cookie
streams, temporary files, account lookup, signals, calendar/timezone changes, wide strings,
encoding boundaries, regex captures and filename patterns. Checks remain active with `-prod`.
Integer absolute-value/division APIs and child waiting use the public C ABI. Wait checks include
status guards, resource usage, nonblocking calls, stopped/continued events and `WNOWAIT`.
System V IPC checks create private semaphore sets, shared memory and message queues, verify native
metadata and error paths, exercise process-shared writes and `SEM_UNDO`, and remove every object.
Directory checks grow result vectors, preserve selector callback errors correctly, exercise native
and 64-bit record callbacks, empty selections, reverse ordering and version sorting. String/process
checks cover long case-insensitive searches, token state, effective access, groups and short sleeps.
Fortify checks use volatile native function pointers so C compiler folding cannot bypass the
public ABI. They cover buffer guards, overlap, padded strings, wide elements, streams, descriptor
output, short lines with oversized input limits, variadic formatting and truncation.
Fourteen isolated children
must abort for invalid bounds, multiplication overflow, missing open modes and unsafe `%n`.
Mapped format tests distinguish fully read-only storage from a writable trailing mapping.
An ELF constructor also calls checked stdio before V's module initializers.
Two additional replacement-runtime children call the global and local stack-check failure
entry points; both must terminate with `SIGABRT`. The hosted reference checks the global entry.
Native floating checks cover zero/sign, infinities/NaNs, subnormal and exponent boundaries,
extended precision, and 2,048 double bit patterns under every rounding mode. They check both
amd64 floating control units, invalid-mode preservation and independent thread rounding.
Environment checks include default sentinels, saved flag widths, live x87 registers, hold/update,
exception-mask controls, nontrapping flag restoration and real `SIGFPE` in isolated children.
Raw x87 and IEEE binary128 vectors check normalization, padding guards and sign/payload bits
on either host; they do not substitute for exercising another architecture's calling convention.
Locale checks cover float/double/long-double parsing, GNU aliases, Unicode wide date literals,
ISO week dates, wide-character capacities, large year padding and caller-locale restoration.
The syslog probe redirects the runtime's connection to a private Unix socket; it never uses
its normal logger destination.

```sh
./v -syscalls -vlibc -no-system-libc -prod -cc gcc -o /tmp/vpac_runtime_probe \
  vlib/builtin/testdata/freestanding_compat/vpac_runtime.c.v
/tmp/vpac_runtime_probe
```

The same C probe can run against the host libc with `-DV_RUNTIME_HOSTED_ORACLE`.
That mode omits the logger test because libc's internal connection bypasses linker wrapping.
It also omits replacement-only GNU alias checks. Link the hosted C probe with `-pthread -lm`.
Other output, including capture offsets, can be compared directly. The native runtime test
covers the logger separately. The single-byte encoding table generator uses Python's standard
codec mappings; undefined byte values are preserved as invalid entries.

The dynamic lookup regression additionally unwinds through two real shared-library call frames.
It compares object bounds and EH/SFrame metadata with public program headers, checks simultaneous
lookups, unchanged failure outputs, and errno/loader-error preservation. A separate explicit static
fixture exercises the main-image lookup without a dynamic loader. Both run in debug and production:

```sh
./v -silent vlib/v/compiler_tests/vlibc_dynamic_lookup_test.v
```

## Native HTTPS and package-library profile

`build_vpac.py` extends an existing complete X11/font native prefix with curl, HTTP/2,
MbedTLS, libarchive and compression libraries, ACL, GPGME/Assuan/gpg-error, seccomp and OpenLibm.
`vpac_sources.json` pins the package libraries. The X11, package and media profiles all use the
OpenLibm source hash in `media_toolchain_sources.json`. Builds run one job by default.
The output contains genuine shared libraries with distinct SONAMEs and `$ORIGIN` search paths;
all dependency entries are retained and checked for an accidental host libc dependency.
Public export checks use compiled ELF visibility, including intentionally hidden internal APIs.

```sh
nice -n 15 python3 vlib/builtin/testdata/freestanding_compat/build_vpac.py \
  /tmp/vlibc-vpac-build --prefix /tmp/vlibc-vpac-prefix --jobs 1
VLIBC_PREFIX=/tmp/vlibc-vpac-prefix ./v -syscalls -vlibc -no-system-libc -cc gcc \
  -o /tmp/native_tls_probe vlib/builtin/testdata/freestanding_compat/native_tls.c.v
python3 vlib/builtin/testdata/freestanding_compat/native_tls_probe.py /tmp/native_tls_probe
```

The builder keeps native-library MbedTLS headers private and namespaces the compiled crypto
symbols. V's bundled TLS implementation and curl's independently configured TLS implementation
can therefore coexist without exchanging incompatible internal objects. Curl enables HTTPS,
HTTP/2 and compression; optional SSH, HTTP/3, LDAP, GSSAPI, IDN and libpsl integrations are not
part of this profile. Existing X11/font libraries are preserved. `--install` publishes a complete
immutable profile through the managed native-prefix symlink; use an explicit prefix while testing.

The integration probe creates a private TLS server and temporary GPG keyring. It verifies both
V HTTPS and curl HTTP/2 concurrently, rejects a bad certificate, round-trips six archive filters,
checks valid and tampered signatures, and installs a seccomp filter in a separate test process.
It inspects the process map for libc and tests lookup of genuinely linked shared dependencies.
External GPG helpers are separate processes; they use their system runtime.

For the actual vpac checkout, build against the same explicit profile and run the private
application probe. It checks help, IPC framing/errors, and the process map. A local search
first reads an empty database, then finds one of two packages by description and checks the
reported source, version and description. The probe uses its own configuration, filesystem
root, database, log, keyring and cache.

```sh
VLIBC_PREFIX=/tmp/vlibc-vpac-prefix ./v -syscalls -vlibc -no-system-libc -prod -cc gcc \
  -o /tmp/vpac /path/to/vpac
python3 vlib/builtin/testdata/freestanding_compat/vpac_profile_probe.py \
  --libc-free /tmp/vpac
```

To exercise the regular V runtime and its default collector, use `-shim-libc` in place of
`-syscalls -vlibc -no-system-libc` in the build command. The regular runtime can link `-lm`
against the prefix's `libm-vlibc.so`. The same process-map check applies to both builds.

For a persistent installation, start with the shared X11 profile and add the package libraries.
V discovers the installed profile on subsequent builds, including when invoked from vpac.

```sh
python3 vlib/builtin/testdata/freestanding_compat/build_x11.py \
  /tmp/vlibc-x11-shared --shared --install
python3 vlib/builtin/testdata/freestanding_compat/build_vpac.py /tmp/vlibc-vpac --install
cd /path/to/vpac
v -cc gcc -shim-libc -o /tmp/vpac .
```

## veb server integration

```sh
./v -syscalls -vlibc -no-system-libc -cc gcc -o /tmp/veb_runtime \
  vlib/builtin/testdata/freestanding_compat/veb_runtime.v
python3 vlib/builtin/testdata/freestanding_compat/veb_runtime_probe.py \
  --libc-free /tmp/veb_runtime
```

The server probe checks text and JSON routes, request bodies, static files, missing routes,
concurrent clients and graceful shutdown. For the libc-available syscall mode,
omit `-no-system-libc`
and the probe's `--libc-free` option. Add `-prod` before the V source for production validation.

## Vast terminal integration

Build Vast and run its existing X11 test on a private display. This requires Linux, Xvfb,
Python 3, xdotool, xclip and a monospace font. The test runs a child on a real PTY, checks
rendered pixels after resizes, copies selections, pastes 90,000 bytes, sends keyboard input,
and checks the child's exit status. The probe also checks the terminal's loaded libraries.

```sh
vast_src="$HOME/Projects/vast"
./v -cc gcc -syscalls -o /tmp/vast-syscalls "$vast_src"
xvfb-run -a python3 vlib/builtin/testdata/freestanding_compat/vast_runtime_probe.py \
  /tmp/vast-syscalls --project "$vast_src"
./v -cc gcc -prod -bare -shim-libc -o /tmp/vast-shim "$vast_src"
xvfb-run -a python3 vlib/builtin/testdata/freestanding_compat/vast_runtime_probe.py \
  /tmp/vast-shim --project "$vast_src" --libc-free
```

The second build uses the installed vlibc X11/font profile. With `-prod -syscalls -vlibc`,
run the same probe with the system libc expectation (omit `--libc-free`).

To check collection during redraws, build with the explicit Boehm option. This probe drives
a child on Vast's PTY and waits for each alternating background color to reach the X11 window.
It samples the terminal's private dirty pages every 200 repaints of the same cells.

```sh
./v -prod -syscalls -no-system-libc -vlibc -gc boehm -o /tmp/vast-boehm "$vast_src"
xvfb-run -a python3 vlib/builtin/testdata/freestanding_compat/vast_redraw_probe.py /tmp/vast-boehm
```

## Vendetta desktop integration

### Native C++ prerequisites

`build_media_toolchain.py` builds the Linux amd64 math/C++ prerequisites into a separate prefix.
It preserves the X11/package profile and adds real shared OpenLibm, quadmath, GCC unwinding and
libstdc++ libraries. `media_toolchain_sources.json` pins source archives and checksums. C++ sources
use `__NO_INLINE__` so system headers cannot inline private libc FILE access into native objects.
The builder supplies GCC's generated POSIX-thread selection for standalone configuration, cleans
objects after configuration changes, and audits exports including GNU-unique data symbols.

The installed compiler supplies its PIC EH archive and C++ DSO startup objects. Its actual shared
unwinder exports identify public ABI entries; register-preserving jump glue exposes those entries
without changing hidden implementations. The system shared unwinder is not linked into the result.
V supplies public GNU floating-environment APIs; OpenLibm's BSD helper environment stays internal.
Its compiled helper symbols are also namespaced in the static archive, keeping GNU and BSD
environment storage separate for explicit static consumers.
Quadmath retains full binary128 parsing/formatting. GNU printf registration, private wide-locale
metadata and i18n number rewriting are disabled for quadmath's optional extensions.

```sh
nice -n 15 python3 vlib/builtin/testdata/freestanding_compat/build_media_toolchain.py \
  /tmp/native-media-toolchain --base /tmp/vlibc-vpac-prefix \
  --prefix /tmp/native-media-prefix --jobs 1
nice -n 15 python3 vlib/builtin/testdata/freestanding_compat/test_media_toolchain.py \
  /tmp/native-media-prefix /tmp/native-cpp-probe --v ./v
```

The sequential debug/production probe checks C++ constructors before V initialization, streams,
Unicode locales and date facets, filesystem I/O, exceptions across shared libraries, aligned
allocation, mutexes, condition variables, thread-local destructors and DSO finalization. Binary128
cases check precision beyond long double, extreme exponents, truncation and invalid-format aborts.
Live process maps must contain the native dependencies and exclude libc and the system C++ runtime.
The driver exports the complete runtime and checks strong imports across the entire native prefix,
including libraries the C++ fixture does not itself load, using compiled ELF symbol tables.
This prerequisite profile is not a complete native media/graphics stack and is not published as
the default installation.

### Native subtitle, audio and codec prerequisites

`build_media_codecs.py` extends the C++ prerequisite prefix with FriBidi, HarfBuzz and its enabled
companion libraries, libass, ALSA and FFmpeg. Source archives are pinned in
`media_codecs_sources.json`. Each compiled archive becomes a real shared library with its complete
public exports and explicit native dependencies. FFmpeg enables its built-in codecs, formats,
filters and protocols, native subtitle/font backends, compression and namespaced Mbed TLS HTTPS.
Host hardware/codec libraries, ICU, Graphite, libunibreak and Python mixer plugins are excluded.
ALSA configuration is installed inside the output prefix. No system configuration is overwritten.
When NASM is missing, the builder compiles its pinned source into the work directory's `tools`
prefix, retaining optimized assembly in libass and FFmpeg without installing system packages.
FFmpeg's own generated export maps and shared-compatible archive targets retain its public ABI
and duplicate the private tables each shared consumer needs. Public pkg-config link lines select
the DSOs; their private flags retain the full native archive closure for static consumers.

```sh
nice -n 15 python3 vlib/builtin/testdata/freestanding_compat/build_media_codecs.py \
  /tmp/native-codecs-build --base /tmp/native-media-prefix \
  --prefix /tmp/native-codecs-prefix --jobs 1
nice -n 15 python3 vlib/builtin/testdata/freestanding_compat/test_media_codecs.py \
  /tmp/native-codecs-prefix /tmp/native-codecs-probe --v ./v
```

The same integration assertions run against host libraries first, then native debug and production
builds. The probe shapes Arabic, reorders Hebrew, renders actual subtitle masks, writes ALSA PCM
through a private null-device configuration, decodes a generated lossless video/audio clip, checks
RGB pixels, filters/scales frames, resamples audio and seeks. Process maps are inspected both before
and after native work. The clip changes from red to blue at a one-second keyframe and includes a
yellow corner marker; pixel checks verify the filter's horizontal flip and exact post-seek frames.
This profile remains separate from the installed default; native graphics,
libplacebo and libmpv are additional requirements for a complete Vendetta runtime.
Use `--modes hosted` to validate the fixture against host libraries alone, or
`--modes debug prod` to rerun only the native configurations.

### Desktop playback and chooser probe

The GUI probe generates a silent test clip, starts its own Xvfb display and session bus, and
launches Vendetta with `--ao null` in private XDG directories. It checks the active audio output
and sample rate, changed video pixels after paused seeks, resumed playback and a normal window
close. `--file-dialog` additionally selects a
filename containing spaces and Unicode through the actual zenity/GTK chooser, then cancels
a second chooser and checks that its child is reaped and the current path is preserved.
It requires Python 3, FFmpeg, Xvfb, glxinfo, xdotool, xclip, dbus-daemon and zenity.
Software rendering uses one llvmpipe worker; builds and GUI probes should run separately.

```sh
./v -syscalls -cc gcc -d ui2_custom_rendering -o /tmp/vendetta-syscalls /path/to/vendetta/src
python3 vlib/builtin/testdata/freestanding_compat/vendetta_runtime_probe.py \
  /tmp/vendetta-syscalls --project /path/to/vendetta --file-dialog \
  --output /tmp/vendetta-runtime.json
```

The system mpv/graphics libraries still depend on libc. The optional `--libc-free` check is for
a future complete native media profile; omitting automatic `-lc` alone does not satisfy it.
