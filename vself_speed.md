# V self-compilation speed

Every compiler fix must preserve or improve self-compilation efficiency. Agents must compare
retired instructions before and after compiler changes and resolve regressions before merging.
Keep correctness tests and generated-output checks alongside performance measurements.

## Why the reported scores differed

The earlier **115,328 -> 95,877 instructions/line** comparison used compiler sources and input
at `257b514a`, Apple Clang 21, and a **560,382-line** fixture. Its two roles were ordinary
production and PGO production builds.

The later **124,706.52** score was an **unprofiled baseline**, not its optimized result.
Homebrew Clang 23 wrote a profile that the selected Apple LLVM 21 merger could not read, so
self-building fell back to an ordinary production build. Fixing that tool lookup gave
**101,502.13** in the same comparison, on the newer **559,990-line** fixture.

Those historical scores changed compiler revision, input, and C toolchain. They must not be
presented as a continuous performance series. A fresh revision check, described below, kept
input and Apple Clang 21 fixed: **97,584.84 -> 105,074.91**, or **7.68% more instructions**.
This confirms a revision-associated increase as well as the reporting error; it does not isolate
which commit or build-training difference caused that increase.

## Metric and scope

Use **retired instructions per parsed source line** to express compiler work; lower is better:

```text
instructions per source line = retired instructions / parsed source lines
```

Also retain total retired instructions per fixed self-compilation. With the same source fixture,
both metrics give the same percentage improvement. Instructions per second depends on hardware,
scheduling, and instruction mix, so it is not a compiler-efficiency score.

Count physical lines in the `.v` and `.vh` files actually parsed, including builtins and imported
modules. Include blank lines and comments: count newline characters and add one for a nonempty
final line without a newline. Empty files contribute zero. Use the compiler's `parsed .v lines`
plus `parsed .vh lines` reports from a separate verbose run of the same workload. Do not use the
entry file alone, generated C, or every source file in the repository as the denominator.

Keep the fixture and counting rule fixed. Different lines can require very different amounts of
checking, generic instantiation, and code generation; instructions/line is not a fair ranking of
unrelated programs. Adding blank lines must not count as an optimization. When updating the
fixture, establish a new baseline instead of comparing normalized scores across fixture versions.

Compare on the same machine and architecture. Keep compiler build options identical unless
changing the build recipe is the optimization being evaluated; record that change. Record elapsed
time, CPU cycles, and peak resident memory too. Fewer instructions do not guarantee lower latency.

Retired instructions are counted for the measured process. Other processes do not contribute
their instructions, so an idle host is not required. These are dynamic execution counts: locks,
retries, and other runtime paths can still introduce variation. Use `VJOBS=1` and repeated pairs
to assess consistency. Record competing work as context for elapsed time and cycles; collect
latency claims separately under controlled load. Apple documents the per-thread and per-task
accounting in [XNU Recount][recount].

[recount]: https://github.com/apple-oss-distributions/xnu/blob/main/doc/observability/recount.md

The workload compiles the full `cmd/v` compiler through C generation, matching the V stages of
`v self`. It excludes the external C compiler, linker, replacement of `./v`, and execution of
the resulting compiler. It includes parsing, checking, transforms, and C generation. Use one
worker for the instruction comparison; validate default parallelism separately.

## Measurement instructions for agents

1. Start from a clean baseline and preserve its commit. Build baseline and candidate compilers
   outside measured runs with the same production command and C toolchain:

   ```sh
   env -u VFLAGS -u VEXE SOURCE_DATE_EPOCH=0 V_MACOS_V3_NO_FALLBACK=1 \
     ./v -prod -cc clang self
   ```

2. Keep an unchanged checkout as the source fixture, including its standard library and native
   dependencies. Copy each built compiler to the same `./v` path in that fixture in turn.
   Use identical source, executable, working-directory, and output paths for both roles.
   Record the fixture commit and content hashes, binary hashes, OS, hardware, toolchain,
   build commands, flags, and effective environment. Never measure a stale binary.
3. From the fixture root, collect the source-line denominator outside measured runs:

   ```sh
   benchmark_out=$(mktemp -d "${TMPDIR:-/tmp}/vself-speed.XXXXXX")
   env -u VFLAGS -u VEXE -u LLVM_PROFILE_FILE VJOBS=1 SOURCE_DATE_EPOCH=0 \
     V_MACOS_V3_NO_FALLBACK=1 \
     ./v -nocache -gc none -prealloc -compile-backend fastc -building-v -v \
       -o "$benchmark_out/self.c" cmd/v \
     > "$benchmark_out/inventory.stdout" 2> "$benchmark_out/inventory.stderr"
   ```

   Require success, sum the two parsed-line reports, and preserve the log and source-file list.
   Verify that baseline and candidate parse the same files and line counts. Repeat this inventory
   when changing the fixture, target, defines, backend, or other input-compilation flags.
   Verbose reporting performs extra work; exclude this run from instruction statistics.
4. Measure this command on macOS arm64, without verbose reporting:

   ```sh
   env -u VFLAGS -u VEXE -u LLVM_PROFILE_FILE VJOBS=1 SOURCE_DATE_EPOCH=0 \
     V_MACOS_V3_NO_FALLBACK=1 \
     /usr/bin/time -l -o "$benchmark_out/time.txt" \
     ./v -nocache -gc none -prealloc -compile-backend fastc -building-v \
       -o "$benchmark_out/self.c" cmd/v \
     > "$benchmark_out/stdout.txt" 2> "$benchmark_out/stderr.txt"
   benchmark_status=$?
   printf '%s\n' "$benchmark_status" > "$benchmark_out/exit-status.txt"
   cat "$benchmark_out/time.txt"
   ```

   The measured input is a normal compiler build; `-prod` optimizes the compiler running the
   benchmark, not the input workload. `-nocache` disables V caches, not filesystem caches.
   `V_MACOS_V3_NO_FALLBACK=1` prevents silent fallback to the legacy compiler.
   Reuse one output C path across all paired runs and keep distinct timing/stdout/stderr logs.
5. Run one warmup per role, then ten measured pairs, alternating AB, BA, AB, BA, and so on.
   Run only one benchmark at a time. Do not wait for an idle host or pause unrelated work.
   Other builds and tests do not invalidate process instruction counts. Keep native support
   objects warm for both roles and record that state. Disable tracing and profiling.
   Label elapsed time and cycles collected under uncontrolled load as contextual measurements.
6. Require exit status zero and exactly one positive `instructions retired` counter per sample.
   Preserve failed samples but do not count them as a completed comparison. If hardware counters
   are missing or zero, report them as unavailable; do not substitute elapsed time.
7. Verify byte-identical generated C for an internal optimization, allowing only recorded build
   metadata differences. Run the relevant correctness tests and rebuild the working `./v`.
8. Divide each run's retired instructions by the fixed parsed-line count. Report the denominator,
   median instructions/line, total instructions, percentage reduction, median paired instruction
   difference, number of pairs that regressed, elapsed time, cycles, and memory.
   Retain all raw samples and reproduction metadata. Investigate inconsistent pairs; never
   select the fastest run or compare medians from separate sessions as proof of a small gain.

On another platform, use an available hardware retired-instruction counter with the same scope
and protocol; do not compare its absolute counts with this machine's results.

## Current self-compilation fixture

New comparisons use a detached checkout of merged master
`2d1367e589ed30c03325e586a8ddfd3bc96c8c3d`, including its standard library.
It contains **559,990 parsed physical lines in 562 `.v` files**, with no `.vh` files.
The 90,000 instructions/line target is **50,399,100,000 instructions** for this fixture.
Keep this source and denominator fixed across baseline and candidate runs.

The older 257b fixture below is incompatible with the merged compiler's sum-type representation:
it emits incomplete recursive C union fields for its old compiler types. A C-file hash match
alone did not catch this. The new fixture's generated C passes Clang syntax checking. Validate
generated C when changing the fixture or compiler representation; do not accept counts for an
invalid output. This establishes a new baseline; do not compare normalized scores across fixtures.

An exploratory three-pair run measured 105,611.36 instructions/line for merged master and
105,762.28 for local-storage lookup guards. Generated C matched in every run. The guards showed
no convincing gain and are excluded from the optimization. Three pairs do not establish a small
regression or a completed ten-pair comparison. Logs remain under
`/Users/alex/code/v3-self-90k-artifacts/guard-probe*`.

The new fixture is `/Users/alex/code/v3-self-90k-fixture`. Its 8,498-file source manifest
uses the encoding described below and has SHA-256:
`f53a9b162657ba667fde1d8a00cd6f59c0f7e6c7f847985894d61189122a2966`.
Its source inventory, syntax-check logs, build logs, binaries, and instruction samples remain in
`/Users/alex/code/v3-self-90k-artifacts/`.

## Semantic interning cache comparison

The first source optimization on master `9ca360c4ff` adds an atomic cache of immutable canonical
semantic types. Hits verify complete structural equality before bypassing recursive hashing,
locking, and map lookup. Promotion clears the cache before worker arenas are released. The
checker also uses the cheap fingerprint for its existing type-name cache, retaining its full
structural-equality check.

Both roles use successful Clang 23 PGO builds with `-mcpu=apple-m1`, the same build recipe,
and the unchanged 559,990-line fixture. The candidate is the first cache variant, before the
subsequent source-spelling and child-fingerprint experiments.

| Median metric | Before, one worker | Cache, one worker | Before, default | Cache, default |
| --- | ---: | ---: | ---: | ---: |
| Retired instructions | 57,045,612,737.0 | 55,295,951,631.0 | 61,027,236,671.0 | 59,242,819,343.0 |
| Instructions/parsed line | 101,868.98 | 98,744.53 | 108,979.15 | 105,792.64 |
| CPU cycles, billions | 27.0600 | 26.8994 | 28.7436 | 28.6683 |
| Peak resident memory, MiB | 1,118.17 | 1,118.55 | 1,011.12 | 1,014.97 |
| Elapsed seconds | 10.6821 | 9.4104 | 2.7068 | 2.6945 |

Retired instructions decreased **3.07% with one worker** and **2.92% with default parallelism**.
All ten pairs in each mode improved. Median paired instruction differences were
`-1,799,425,264` and `-1,775,404,206.5`, respectively. All 44 samples succeeded and generated
byte-identical C within each mode. C passed Clang syntax checking. Elapsed time and cycles
were collected under uncontrolled host load. **The 90,000 instructions/line target is not met.**

The 151 type tests passed, including borrowed-payload mutation, concurrent growth, and arena
promotion checks. Full compiler coverage and further optimization are still in progress.
Commands, environment, raw counters, and output checks are retained as
`interner-cache-matched-*` and `interner-cache-syntax-validation.json` in the artifacts directory.
The baseline executable SHA-256 is
`575ec55f8f553cfea6bff81ad937192687a840d84cc0f7e8832327804be5b8b7`;
the candidate executable SHA-256 is
`22e35698c36f05c7009b2516cda9270985366404230fabdf2399c16f178a3572`.

## Follow-up cache comparison with Apple Clang 21

A fresh comparison on master `9ca360c4ff` used Apple Clang 21 PGO for both roles and the same
559,990-line fixture. The candidate combines the semantic interning cache with source-spelling
content fingerprints, shallow semantic child fingerprints, and a reference receiver for
`Type.source_type_id`. Fingerprints still require complete equality checks before reuse.

| Median metric | Baseline, one worker | Combined cache, one worker |
| --- | ---: | ---: |
| Retired instructions | 59,064,336,077.0 | 57,522,052,933.5 |
| Instructions/parsed line | 105,473.91 | 102,719.79 |
| CPU cycles, billions | 29.1419 | 28.4944 |
| Peak resident memory, MiB | 1,117.54 | 1,118.23 |
| Elapsed seconds | 33.9576 | 34.9625 |

The combined change used **2.61% fewer instructions**, with **0/10 regressing pairs** and a
median paired difference of `-1,529,766,769` instructions. All 22 runs succeeded and emitted
byte-identical C. Elapsed time and cycles reflect uncontrolled host load. This is a matched
source-change comparison; its absolute scores must not be compared with the Clang 23 sessions.
The 90,000 instructions/line target is still unmet. Default-parallel validation is pending.

Raw samples, binary hashes, commands, environment, and output checks are retained as
`reference21-content-matched-serial-*` in the artifacts directory above. This bundle comparison
does not establish an independent benefit from each additional fingerprint change.

## LLVM toolchain lookup comparison on latest master

Measured on 2026-10-21 with compiler sources from master `9ca360c4ff`, the toolchain lookup
fix at `8a6e8a32dc`, and the same 559,990-line fixture. Both compilers were rebuilt in place
with the command above, using Homebrew Clang 23.1.0 and `-cflags '-mcpu=apple-m1'`.
The baseline again fell back after the LLVM 21/23 profile-format mismatch; the candidate
completed PGO. This compares the tool lookup fix on the same compiler sources, so upstream
changes between master versions are not attributed to that fix.

| Median metric | Before, one worker | After, one worker | Before, default | After, default |
| --- | ---: | ---: | ---: | ---: |
| Retired instructions, billions | 68.9401 | 56.3825 | 73.8913 | 60.6981 |
| Instructions/parsed line | 123,109.57 | 100,684.76 | 131,951.04 | 108,391.44 |
| CPU cycles, billions | 23.0552 | 20.1368 | 32.7966 | 29.9206 |
| Peak resident memory, MiB | 1,116.04 | 1,118.15 | 1,009.52 | 1,014.15 |
| Elapsed seconds | 5.4194 | 4.7996 | 1.2632 | 1.1951 |

Instruction reductions were **18.22% serial** and **17.85% default**, with **0/10 regressing
pairs** in each mode. Median paired instruction differences were `-12,579,586,975.5` serial
and `-13,218,242,894.5` default. **The 90,000 instructions/line target remains unmet.**
All 44 runs succeeded and generated byte-identical C within each mode; both outputs passed
Clang syntax checking. Elapsed time and cycles were collected under uncontrolled host load.
The PGO, ordinary self-build, and cache-bypass tool tests passed, as did the hello-world smoke test.

Raw samples, complete environments, commands, build logs, binary hashes, syntax checks, and
validation logs are retained as `latest2-*` in the artifacts directory above. Generated C
hashes match the preceding master comparison below. SHA-256 values:

```text
baseline executable, latest2-before-v
8cdaf2fd18d6abd30e12e67953914abfa1d0b3e733211c4509b316095de71b9b
candidate executable, latest2-after-v
575ec55f8f553cfea6bff81ad937192687a840d84cc0f7e8832327804be5b8b7
merged training profile
93e991a1f4f6952c4645d0dab67a4da1a897fca48c647dea5dcc5c53f9f1db1b
```

## Compiler revision check on identical input

A separate 2026-10-21 comparison held the 559,990-line fixture, source-file list, Apple Clang 21,
production settings, and PGO recipe fixed. The older binary uses compiler sources at `257b514a`;
the newer binary uses merged sources at `2d1367e589`. Each build trained on its own revision's
compiler source, following the same self-build recipe. The older binary successfully compiles
the newer fixture to C, which makes this direction of comparison possible.

| Median metric | Older compiler | Merged compiler |
| --- | ---: | ---: |
| Retired instructions | 54,646,536,102.5 | 58,840,900,876.5 |
| Instructions/parsed line | 97,584.84 | 105,074.91 |
| CPU cycles, billions | 23.5851 | 25.1845 |
| Peak resident memory, MiB | 1133.02 | 1117.52 |
| Elapsed seconds | 7.9881 | 8.9251 |

The merged compiler executed **7.68% more instructions** between medians, with more work in
**10/10 pairs**. The median paired increase was `4,174,866,314` instructions. All 22 runs
succeeded and each role produced repeatable C. Both C outputs passed Clang syntax checking.
They intentionally differ across roles because compiler revisions changed sum-type representation;
this is a revision diagnostic, not an output-preserving internal optimization comparison.
It establishes increased work for these compiler builds without isolating a particular commit.
Elapsed time and cycles were collected under uncontrolled host load.

Thus the earlier reporting mixed an unprofiled baseline with a PGO result, but a compiler-version
increase also exists on matched input. The historical 95,877.04 result still uses the older
560,382-line fixture; attributing the remaining fixture effect requires a separate matched check.
Do not explain the entire discrepancy as denominator changes or assume that no regression exists.

Logs, inventories, counters, commands, environments, and syntax checks are retained as
`revision-diagnostic-*` in the artifacts directory above. The 562-file inventory matches exactly
between roles. SHA-256 values:

```text
older compiler executable
e554d76a9ad101c03c74864b5d428cef6f7b1df29fd11dad8ab43e74e60cbde4
merged compiler executable
7aab1e75525e5966566d7fe30debb543aaaeea68159dcd94047b46b7ada3788f
older compiler generated C on the current fixture
abb6372f6afc7892a15db0843ad4cca9fa2ca638ef331704866d59fb6d72d50d
merged compiler generated C on the current fixture
88583a0ca0e4f59877f7c2b7a6cb4260c046c4737a61c665b62af265344bba8d
```

## Matched Clang 23 comparison before the latest master rebuild

Measured on 2026-10-21 with compiler sources from merged master `2d1367e589` and the fixed
559,990-line fixture above. Both roles selected Homebrew Clang 23.1.0 and `-mcpu=apple-m1`.
The baseline selected Apple's LLVM 21 profile merger, which rejected Clang 23's version-11 raw
profile and triggered an ordinary production build. The candidate selected Clang 23's merger
and completed PGO. Both final binaries contain no profiling runtime instrumentation.

The same production self-build command was used in each checkout, outside measured runs:

```sh
env -u VFLAGS -u VEXE -u LLVM_PROFILE_FILE -u VJOBS SOURCE_DATE_EPOCH=0 \
  V_MACOS_V3_NO_FALLBACK=1 ./v -prod -cc /opt/homebrew/opt/llvm/bin/clang \
  -cflags '-mcpu=apple-m1' -o /absolute/path/to/role-v self
```

Each mode used one warmup per role and ten alternating measured pairs. All 44 runs succeeded;
generated C matched byte for byte within each mode. Elapsed time and cycles reflect shared-host
load and do not establish a latency improvement.

| Median metric | Before, one worker | After, one worker | Before, default | After, default |
| --- | ---: | ---: | ---: | ---: |
| Retired instructions, billions | 69.8344 | 56.8402 | 74.4558 | 61.1362 |
| Instructions/parsed line | 124,706.52 | 101,502.13 | 132,959.12 | 109,173.73 |
| CPU cycles, billions | 33.1496 | 28.9486 | 33.3547 | 30.3399 |
| Peak resident memory, MiB | 1116.16 | 1118.05 | 1008.88 | 1010.32 |
| Elapsed seconds | 21.2254 | 18.6263 | 5.4424 | 5.5684 |

The median instruction reductions were **18.61% serial** and **17.89% default**, with
**0/10 regressing pairs** in each mode. Median paired differences were `-12,984,267,344.5`
and `-13,337,504,752` instructions, respectively. The 90,000 target was not reached.
Logs and samples are `clang23-matched-{serial,default}*` in the artifacts directory above;
`clang23-before-build.log` records the version mismatch and fallback, while
`clang23-after-build.log` records the successful PGO build.

The optimized count is higher than the historical 95,877.04 result below, but these sessions
do not isolate a compiler regression. The historical result used Apple Clang 21, compiler
sources at `257b514a`, and a different source fixture.
Both compiler implementation and input changed after the tagged-union merge. The fixture has
only 392 fewer lines, so the denominator alone does not explain the difference. Isolating the
causes requires matched comparisons; comparing these two sessions cannot attribute the increase.
In particular, 124,706.52 is the current **unprofiled baseline**, not the optimized result.

SHA-256 values for this comparison:

```text
baseline executable, clang23-before-v
1541c85d77b10337467caacbc6cf67314a1635d78ce5c9e20e1b8b7f1fb6d454
candidate executable, clang23-after-v
c5ad981e88dc2f22fd0bd97042094e89a55ac85ee4dcd75e2ffa2ac86714edaa
generated C, every serial run
88583a0ca0e4f59877f7c2b7a6cb4260c046c4737a61c665b62af265344bba8d
generated C, every default-worker run
04603de3a32496290c63a567e46069509fd0f985906a40b9696303f14a337158
```

## Historical measurement status

The PGO comparison below records process instruction counts under shared-host load on the old
fixture. The earlier quiet-host gate was too strict: unrelated compilations do not invalidate
these instruction measurements. All ten paired differences favored PGO in each mode, with
matching source inventories and generated C. Elapsed times and cycles reflect uncontrolled
host load and do not establish a latency improvement. These results describe the older compiler
and fixture, not the current self-compilation baseline.

The 2026-10-21 attempt that waited 120 seconds for an idle host started no measurements.
Its logs remain in `/Users/alex/code/v3-self-speed-artifacts/quiet-20261021-attempt1.log`
and `quiet-20261021-attempt1-guard.json`; an idle host is no longer an acceptance requirement.

### Historical PGO instruction comparison

Measured on 2026-10-20. Baseline: merged master
`257b514a303f4d302ed23bbac1d51a2ca9624a66`. Candidate: that commit with the Clang production
self-build changes later merged in [#30962](https://github.com/vlang/v/pull/30962).
The compiler source compiled into both binaries is unchanged;
the intentional build difference is profile-guided optimization (PGO).
The candidate was rebuilt and remeasured after correcting atomic profile collection and
isolating the training environment.

Both used the production command above, with Clang's large-unit `-O2 -flto` settings, GC disabled,
preallocation, and FastC included. The baseline made one ordinary optimized compiler. The candidate
made a compiler with atomic profile counters, trained it on uncached self-compilation with
default workers and `VJOBS=1`, merged the profiles, and rebuilt with `-fprofile-use`. The compiler
contains no profiling instrumentation. This comparison evaluates the resulting compiler's execution,
not the cost of producing it.

The session measured 16.87% fewer instructions with one worker and 16.40% fewer with default
workers. These are instruction-count reductions, not elapsed-time improvement claims.
All 44 runs, including warmups, exited successfully and generated byte-identical C.
The fixed input contains **560,382 parsed physical lines in 555 `.v` files**, with no `.vh` files.
The baseline and candidate report the same source closure and line count. Normalizing the retained
instruction samples by this denominator gives the same 16.87% and 16.40% reductions.

| Median metric | Baseline, one worker | PGO, one worker | Baseline, default | PGO, default |
| --- | ---: | ---: | ---: | ---: |
| Retired instructions, billions | 64.6278 | 53.7278 | 68.9180 | 57.6124 |
| Parsed physical source lines | 560,382 | 560,382 | 560,382 | 560,382 |
| Retired instructions/source line | 115,328.08 | 95,877.04 | 122,983.97 | 102,809.22 |
| Elapsed seconds | 25.6428 | 21.7045 | 3.2356 | 2.9937 |
| CPU cycles, billions | 28.4689 | 25.4029 | 30.6580 | 27.2978 |
| Peak resident memory, MiB | 1129.77 | 1131.19 | 998.66 | 1004.67 |

Median paired instruction differences, candidate minus baseline:
`-10,938,181,556` with one worker and `-11,360,585,289.5` with default workers.
Instruction regressions: **0/10 pairs** in each mode, for both total instructions and
instructions per source line.

Process snapshots recorded unrelated compilations elsewhere on the shared host. Retain them
as context for the elapsed-time and cycle measurements. Compare subsequent compiler changes
against a freshly built baseline in the same measurement session, preserving the fixed fixture
and all raw data.

### Production self-build behavior

On non-Windows hosts, `./v -prod -cc clang self` uses PGO when `llvm-profdata` is available.
The profile merger is searched in Clang's reported installation directory, then beside its
resolved executable, then in `PATH` and through `xcrun` on macOS. This avoids a fallback when
a different LLVM release cannot read the selected compiler's raw profile format.
This spends more time building the production compiler: two C compilations plus training, rather
than one C compilation. Ordinary `./v self` keeps its existing build path. Explicit `-parallel-cc`,
`-no-prod-options`, explicit `-cflags` profile options, unavailable profiling tools, and other C
toolchains retain ordinary builds. A failed PGO step falls back to an ordinary production build.

Training clears inherited `VFLAGS`, `VEXE`, and `LLVM_PROFILE_FILE`, disables caches and legacy
fallback, and covers both default worker dispatch and the serial path. Profiling uses atomic
counter updates so worker threads do not lose increments. `VJOBS=1` or `-no-parallel` trains only
the serial path. Workspaces are private to each invocation and removed afterward; existing
workspaces and the caller's profile destination are preserved. Profile paths containing spaces
remain a single C compiler argument. The final effective `-cc` selects the PGO toolchain;
command-line options override compiler choices inherited through `VFLAGS`.

### Reproduction metadata

- Host: Apple M5 Max, 18 CPU cores, 128 GiB RAM; macOS 26.5, build `25F71`, arm64.
- Toolchain: Apple Clang 21.0.0 (`clang-2100.1.1.101`), Command Line Tools; matching llvm-profdata.
- Fixture: clean detached checkout of the baseline commit, including its standard library.
  Bundled TCC: `1d0ad0ecf70a91a1df64cebf215e683b1d5aedb5`; native support objects were warm.
- Fixed executable and working directory: `/Users/alex/code/v3-self-speed-fixture/v` and its root.
- Fixed output: `/Users/alex/code/v3-self-speed-artifacts/self.c`.
- Environment: `SOURCE_DATE_EPOCH=0`, `V_MACOS_V3_NO_FALLBACK=1`;
  `VFLAGS`, `VEXE`, and `LLVM_PROFILE_FILE` were unset.
  `VJOBS=1` for serial measurements and unset for default workers. `CC`, `CFLAGS`, and `LDFLAGS`
  were unset. Locale: `LANG=LC_ALL=C.UTF-8`; SDK: Command Line Tools `MacOSX.sdk`.
- Elapsed seconds use Python `time.perf_counter()` around `/usr/bin/time -l`; the original
  two-decimal `time` output is also retained. Older JSON logs include instructions/second as
  historical data; do not use that field to score compiler efficiency.
- Raw timing, stdout, stderr, exit statuses, process snapshots, scripts, build logs, environment,
  binaries, and JSON samples remain in `/Users/alex/code/v3-self-speed-artifacts/`.
  The current comparison uses the `review-serial` and `review-parallel` logs and JSON files.
  Source-line inventories and normalized samples are retained as `stage-probe.log`,
  `loc-inventory.stdout`, and `loc-normalized.json`.
- The validation build inherited `LLVM_PROFILE_FILE` pointing to an existing caller profile;
  PGO completed successfully and preserved that file. The merged training profile is retained as
  `review-code.profdata`, with SHA-256:
  `f9a836d2f0ee04f0f4929d797a77fe61bfd5b0ca03bfff56acf1e0069186a995`.

SHA-256 values:

```text
baseline executable
5e34c408511cbc55280044e1dd8cd8ff8b4bf78454940d72b9d5346ac08de490
candidate executable
e554d76a9ad101c03c74864b5d428cef6f7b1df29fd11dad8ab43e74e60cbde4
generated C, every run in both modes
46a580197213a2619173a5a17a2f0dac81fd56b14306531e23a0122016e2e3b7
fixture source manifest, 8430 files
22eee10913aa354b01af6dfcf92a521c95f7c5d13c4ebba69bab7b4dd1abdef7
```

The fixture manifest contains sorted Git-tracked `.v`, `.vsh`, `.c`, and `.h` files under `cmd/v`
and `vlib`, with each entry encoded as relative path, NUL, hexadecimal file SHA-256, and newline.
The manifest hash covers those bytes. The task worktree is `/Users/alex/code/v3-self-speed`.
The retained `review-source.diff` records the tool changes used to build the measured candidate.

### All paired samples

The warmup pair is excluded from every statistic. Measured order alternated baseline/candidate
for odd pairs and candidate/baseline for even pairs. Counts are integers; elapsed seconds below
are rounded for display only.

One worker:

| Pair | Baseline instructions | PGO instructions | Baseline s | PGO s |
| --- | ---: | ---: | ---: | ---: |
| warmup | 64,797,713,503 | 53,466,693,440 | 10.0697 | 7.8065 |
| 1 | 64,563,117,272 | 53,457,419,214 | 17.3451 | 9.5121 |
| 2 | 64,357,789,151 | 53,434,337,434 | 9.6554 | 8.4718 |
| 3 | 64,608,456,228 | 53,462,161,257 | 8.7556 | 10.0894 |
| 4 | 64,768,758,757 | 53,534,779,015 | 16.6670 | 12.0576 |
| 5 | 64,586,520,655 | 54,052,280,113 | 46.9252 | 67.1949 |
| 6 | 64,932,154,872 | 54,166,175,890 | 42.3820 | 48.6051 |
| 7 | 64,805,895,480 | 53,852,984,085 | 42.0541 | 23.6558 |
| 8 | 64,609,248,554 | 53,703,915,249 | 25.0856 | 30.6926 |
| 9 | 64,646,316,518 | 54,043,118,299 | 26.2001 | 29.3813 |
| 10 | 64,843,362,579 | 53,751,619,879 | 31.3356 | 19.7531 |

Default workers:

| Pair | Baseline instructions | PGO instructions | Baseline s | PGO s |
| --- | ---: | ---: | ---: | ---: |
| warmup | 69,204,428,535 | 57,795,598,290 | 2.9232 | 2.9259 |
| 1 | 69,362,428,389 | 57,674,395,599 | 3.9125 | 3.3833 |
| 2 | 69,217,849,819 | 57,688,811,832 | 3.8882 | 3.3916 |
| 3 | 69,100,209,757 | 57,432,125,863 | 3.5501 | 2.8208 |
| 4 | 68,588,463,206 | 57,304,532,710 | 3.3423 | 2.4053 |
| 5 | 68,736,420,274 | 57,415,457,216 | 3.1290 | 2.3903 |
| 6 | 68,914,207,957 | 57,478,312,333 | 2.8508 | 2.7886 |
| 7 | 68,921,795,238 | 57,767,190,237 | 2.8897 | 3.1666 |
| 8 | 68,751,695,790 | 57,601,275,877 | 2.0765 | 1.9713 |
| 9 | 68,860,826,104 | 57,623,599,267 | 2.6765 | 3.6652 |
| 10 | 69,169,667,117 | 57,769,459,596 | 4.4418 | 4.6444 |
