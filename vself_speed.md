# V self-compilation speed

Every compiler fix must preserve or improve self-compilation efficiency. Agents must compare
retired instructions before and after compiler changes and resolve regressions before merging.
Keep correctness tests and generated-output checks alongside performance measurements.

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
   Run only one benchmark at a time, without concurrent builds or tests. Keep native support
   objects warm for both roles and record that state. Disable tracing and profiling.
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

## Benchmark acceptance status

**No accepted regression baseline is recorded yet.** The earlier measurements below ran alongside
unrelated compilations and do not meet the required quiet-host protocol. They are retained as
rejected observations for audit purposes, not as evidence that the 10% target has been met.
Both paired modes must be rerun without concurrent builds or tests before accepting a baseline.

A guarded attempt on 2026-10-21 waited 120 seconds for a quiet window and still observed 32
compiler or test processes. It started no warmups or measured runs. The attempt and process
records are retained in `/Users/alex/code/v3-self-speed-artifacts/quiet-20261021-attempt1.log`
and `quiet-20261021-attempt1-guard.json`. A quiet host is required to complete this validation.

### Rejected observations from the shared host

Measured on 2026-10-20. Baseline: merged master
`257b514a303f4d302ed23bbac1d51a2ca9624a66`. Candidate: that commit with the Clang production
self-build changes in this PR. The compiler source compiled into both binaries is unchanged;
the intentional build difference is profile-guided optimization (PGO).
The candidate was rebuilt and remeasured after correcting atomic profile collection and
isolating the training environment.

Both used the production command above, with Clang's large-unit `-O2 -flto` settings, GC disabled,
preallocation, and FastC included. The baseline made one ordinary optimized compiler. The candidate
made a compiler with atomic profile counters, trained it on uncached self-compilation with
default workers and `VJOBS=1`, merged the profiles, and rebuilt with `-fprofile-use`. The compiler
contains no profiling instrumentation. This comparison evaluates the resulting compiler's execution,
not the cost of producing it.

The rejected session observed 16.87% fewer instructions with one worker and 16.40% fewer with
default workers. These percentages are not accepted performance claims under the protocol.
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

Process snapshots recorded unrelated compilations elsewhere on the shared host. The absence of
builds or tests from this task did not satisfy the requirement to measure without any concurrent
builds or tests. Neither the instruction reductions nor the elapsed times from this session
establish an accepted baseline. Preserve the raw data and replace this record with quiet-host
measurements before making performance or regression claims.

### Production self-build behavior

On non-Windows hosts, `./v -prod -cc clang self` uses PGO when `llvm-profdata` is available.
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
