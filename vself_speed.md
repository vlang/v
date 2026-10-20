# V self-compilation speed

Every compiler fix must preserve or improve self-compilation efficiency. Agents must compare
retired instructions before and after compiler changes and resolve regressions before merging.
Keep correctness tests and generated-output checks alongside performance measurements.

## Metric and scope

Use **retired instructions per fixed self-compilation** as the primary metric; lower is better.
This measures compiler work without depending on CPU frequency or scheduling. Compare only on
the same machine and architecture, with identical source inputs. Keep compiler build options
identical unless changing the build recipe is the optimization being evaluated; record that change.

Also record instructions per second, calculated for each run as retired instructions divided by
elapsed seconds. This rate still depends on elapsed time and hardware; higher instruction
throughput alone does not demonstrate a compiler improvement. Record elapsed time, CPU cycles,
and peak resident memory too. A reduction in instructions does not guarantee lower latency.

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
3. From the fixture root, measure this command on macOS arm64:

   ```sh
   benchmark_out=$(mktemp -d "${TMPDIR:-/tmp}/vself-speed.XXXXXX")
   env -u VFLAGS -u VEXE VJOBS=1 SOURCE_DATE_EPOCH=0 V_MACOS_V3_NO_FALLBACK=1 \
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
4. Run one warmup per role, then ten measured pairs, alternating AB, BA, AB, BA, and so on.
   Run only one benchmark at a time, without concurrent builds or tests. Keep native support
   objects warm for both roles and record that state. Disable tracing and profiling.
5. Require exit status zero and exactly one positive `instructions retired` counter per sample.
   Preserve failed samples but do not count them as a completed comparison. If hardware counters
   are missing or zero, report them as unavailable; do not substitute elapsed time.
6. Verify byte-identical generated C for an internal optimization, allowing only recorded build
   metadata differences. Run the relevant correctness tests and rebuild the working `./v`.
7. Report medians, percentage instruction reduction, median paired instruction difference,
   number of pairs that regressed, elapsed time, instructions per second, cycles, and memory.
   Retain all raw samples and reproduction metadata. Investigate inconsistent pairs; never
   select the fastest run or compare medians from separate sessions as proof of a small gain.

On another platform, use an available hardware retired-instruction counter with the same scope
and protocol; do not compare its absolute counts with this machine's results.

## Recorded comparison

Measured on 2026-10-20. Baseline: merged master
`257b514a303f4d302ed23bbac1d51a2ca9624a66`. Candidate: that commit with the Clang production
self-build changes in this PR. The compiler source compiled into both binaries is unchanged;
the intentional build difference is profile-guided optimization (PGO).

Both used the production command above, with Clang's large-unit `-O2 -flto` settings, GC disabled,
preallocation, and FastC included. The baseline made one ordinary optimized compiler. The candidate
made an instrumented compiler, trained it on uncached self-compilation with default workers and
with `VJOBS=1`, merged the profiles, and rebuilt with `-fprofile-use`. The measured compiler
contains no profiling instrumentation. This comparison evaluates the resulting compiler's execution,
not the cost of producing it.

**Result: 17.05% fewer instructions with one worker; 16.57% fewer with default workers.**
Every measured pair improved in both modes, exceeding the 10% instruction-reduction target.
All 44 runs, including warmups, exited successfully and generated byte-identical C.

| Median metric | Baseline, one worker | PGO, one worker | Baseline, default | PGO, default |
| --- | ---: | ---: | ---: | ---: |
| Retired instructions, billions | 64.2030 | 53.2581 | 68.5945 | 57.2311 |
| Instructions/second, billions | 12.6274 | 12.0509 | 76.8676 | 66.5433 |
| Elapsed seconds | 5.0891 | 4.4414 | 0.8920 | 0.8597 |
| CPU cycles, billions | 21.8769 | 19.0591 | 29.8442 | 27.0392 |
| Peak resident memory, MiB | 1129.70 | 1131.31 | 1001.36 | 1007.24 |

Median paired instruction differences, candidate minus baseline:
`-10,959,850,932` with one worker and `-11,363,954,926.5` with default workers.
Instruction regressions: **0/10 pairs** in each mode. Median instructions/second fell 4.57% and
13.43%, respectively, despite the instruction savings. No 10% instruction-rate gain is claimed.

No builds or tests from this task ran during measurement. Process snapshots recorded unrelated
compilations elsewhere on the shared host, so this was **not a quiet-host latency benchmark**.
Treat the elapsed-time and instruction-rate results as observations under contention, not proof
of a stable latency improvement. The instruction reduction held across every pair. Recheck latency
on an idle host before making a wall-clock speed claim, especially for default parallelism.

### Production self-build behavior

`./v -prod -cc clang self` now uses the existing PGO pipeline when `llvm-profdata` is available.
This spends more time building the production compiler: two C compilations plus training, rather
than one C compilation. Ordinary `./v self` keeps its existing build path. Explicit `-parallel-cc`,
`-no-prod-options`, explicit `-cflags` profile options, unavailable profiling tools, and other C
toolchains retain ordinary builds. A failed PGO step falls back to an ordinary production build.

Training clears inherited `VFLAGS` and `VEXE`, disables caches and legacy fallback, and covers
both default worker dispatch and the serial path. `VJOBS=1` or `-no-parallel` trains only the serial
path. Workspaces are private to each invocation and removed afterward; existing workspaces are
not deleted. Profile paths containing spaces remain a single C compiler argument.

### Reproduction metadata

- Host: Apple M5 Max, 18 CPU cores, 128 GiB RAM; macOS 26.5, build `25F71`, arm64.
- Toolchain: Apple Clang 21.0.0 (`clang-2100.1.1.101`), Command Line Tools; matching llvm-profdata.
- Fixture: clean detached checkout of the baseline commit, including its standard library.
  Bundled TCC: `1d0ad0ecf70a91a1df64cebf215e683b1d5aedb5`; native support objects were warm.
- Fixed executable and working directory: `/Users/alex/code/v3-self-speed-fixture/v` and its root.
- Fixed output: `/Users/alex/code/v3-self-speed-artifacts/self.c`.
- Environment: `SOURCE_DATE_EPOCH=0`, `V_MACOS_V3_NO_FALLBACK=1`; `VFLAGS` and `VEXE` unset.
  `VJOBS=1` for serial measurements and unset for default workers. `CC`, `CFLAGS`, and `LDFLAGS`
  were unset. Locale: `LANG=LC_ALL=C.UTF-8`; SDK: Command Line Tools `MacOSX.sdk`.
- Elapsed seconds use Python `time.perf_counter()` around `/usr/bin/time -l`; the original
  two-decimal `time` output is also retained. Each instruction-rate sample uses its own elapsed
  value; the table reports the median of those rates, not the ratio of two medians.
- Raw timing, stdout, stderr, exit statuses, process snapshots, scripts, build logs, environment,
  binaries, and JSON samples remain in `/Users/alex/code/v3-self-speed-artifacts/`.

SHA-256 values:

```text
baseline executable
5e34c408511cbc55280044e1dd8cd8ff8b4bf78454940d72b9d5346ac08de490
candidate executable
a2b09a940a030a39455c603378b13c77b1a77efa5d30564a95d7597f976a8a18
generated C, every run in both modes
46a580197213a2619173a5a17a2f0dac81fd56b14306531e23a0122016e2e3b7
fixture source manifest, 8430 files
22eee10913aa354b01af6dfcf92a521c95f7c5d13c4ebba69bab7b4dd1abdef7
```

The fixture manifest contains sorted Git-tracked `.v`, `.vsh`, `.c`, and `.h` files under `cmd/v`
and `vlib`, with each entry encoded as relative path, NUL, hexadecimal file SHA-256, and newline.
The manifest hash covers those bytes. The task worktree is `/Users/alex/code/v3-self-speed`.
The retained `source.diff` records the tool changes used to build the measured candidate.

### All paired samples

The warmup pair is excluded from every statistic. Measured order alternated baseline/candidate
for odd pairs and candidate/baseline for even pairs. Counts are integers; elapsed seconds below
are rounded for display only.

One worker:

| Pair | Baseline instructions | PGO instructions | Baseline s | PGO s |
| --- | ---: | ---: | ---: | ---: |
| warmup | 64,212,666,706 | 53,225,622,390 | 5.2928 | 4.6144 |
| 1 | 64,532,349,029 | 53,525,125,267 | 5.2758 | 4.4502 |
| 2 | 64,174,179,134 | 53,521,714,310 | 5.1943 | 4.4327 |
| 3 | 64,161,687,410 | 53,257,154,866 | 5.2949 | 4.6621 |
| 4 | 64,245,820,544 | 53,234,662,215 | 5.1130 | 4.4772 |
| 5 | 64,513,512,318 | 53,258,990,103 | 5.0609 | 4.7627 |
| 6 | 64,225,085,203 | 53,263,102,664 | 4.9108 | 4.3478 |
| 7 | 64,276,673,504 | 53,203,461,007 | 5.0652 | 4.2943 |
| 8 | 64,110,616,197 | 53,152,896,872 | 5.0303 | 4.2750 |
| 9 | 64,099,841,929 | 53,163,911,984 | 4.9402 | 4.2283 |
| 10 | 64,180,983,014 | 53,262,691,710 | 5.4979 | 4.5809 |

Default workers:

| Pair | Baseline instructions | PGO instructions | Baseline s | PGO s |
| --- | ---: | ---: | ---: | ---: |
| warmup | 68,759,970,743 | 57,365,265,568 | 0.8840 | 0.6833 |
| 1 | 68,651,916,412 | 57,191,476,005 | 0.8668 | 0.8414 |
| 2 | 68,590,528,091 | 57,322,440,536 | 0.8953 | 0.8405 |
| 3 | 68,575,348,166 | 57,211,559,969 | 0.8984 | 0.8700 |
| 4 | 68,533,567,921 | 57,378,419,085 | 0.8886 | 0.8739 |
| 5 | 68,529,194,359 | 57,193,068,203 | 0.6978 | 0.8742 |
| 6 | 68,552,315,804 | 57,188,194,148 | 0.8962 | 0.8494 |
| 7 | 68,704,536,105 | 57,203,519,391 | 0.8862 | 0.8485 |
| 8 | 68,667,199,639 | 57,250,552,940 | 0.8783 | 0.8876 |
| 9 | 68,737,312,176 | 57,306,738,383 | 0.9061 | 0.8737 |
| 10 | 68,598,475,944 | 57,328,316,006 | 0.9217 | 0.7030 |
