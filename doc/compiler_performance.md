# Compiler performance

On macOS, scoped user builds use up to four compiler jobs when `VJOBS` is unset.
An explicit `VJOBS` retains the requested parallelism. Diagnostics server checks can use
up to eight jobs; compiler self-builds retain their own scheduling policy.

Large uncached C builds check reachable library bodies in successive frontiers. Each body
is checked before its calls are collected, and the reachability queue continues until no
unchecked reachable body remains. Unreachable bodies need no eager call-list collection.
Parallel interface indexing keeps a private query memo per checker for its queue's lifetime,
reusing receiver and candidate lookups without sharing mutable caches between workers.

## Measuring compiler work

Generate C to measure V's parsing, checking, transformation, and code generation separately
from the native C compiler and application execution:

```sh
env -u VFLAGS -u VEXE -u VJOBS SOURCE_DATE_EPOCH=0 V_MACOS_V3_NO_FALLBACK=1 \
  /usr/bin/time -l -o /tmp/vox-time.txt \
  ./v -w -n -enable-globals -nocache -o /tmp/vox.c /path/to/vox
```

Build baseline and candidate compilers with the same production recipe before timing.
Use identical executable, source, output, library, and environment paths for both roles.
Preserve the parsed source inventory and hashes, generated C, binary hashes, and raw logs.
Run one warmup per role, followed by ten serial pairs with alternating order.
Compare median retired instructions and instructions per parsed physical source line.
Also record elapsed time, cycles, and memory. Shared host load makes elapsed time and cycles
contextual measurements. Run a separate comparison with `VJOBS=1` and retain the fixed
self-compilation regression check described in [vself_speed.md](../vself_speed.md).

## Vox comparison, 2026-10-29

Against master `baa94f858fd1fffa8ca341cc71911d61fbf2aad4`, a fixed Vox snapshot uses
**11.8523% fewer retired instructions** with automatic jobs on macOS arm64.
The workload generates C using `-w -n -enable-globals -nocache`; it excludes the native
C compiler. Both roles parse the same **637,670 physical lines in 2,102 V files**, with no
V header files. Sources and embedded resources are frozen outside the checkout.

| Median metric | Baseline | Candidate |
| --- | ---: | ---: |
| Retired instructions | 101,558,020,580.5 | 89,521,055,898 |
| Instructions / parsed line | 159,264.23 | 140,387.75 |
| Peak resident memory, MiB | 3,434.69 | 3,431.62 |
| Peak physical footprint, MiB | 2,828.18 | 3,068.46 |
| Elapsed time, seconds | 24.49 | 25.11 |
| CPU cycles, billions | 54.875 | 48.307 |

All 22 runs succeed, including the two excluded warmups. **0/10 pairs regress**.
The median paired instruction difference is `-11,847,126,420.5`. Peak resident memory
is similar, while peak physical footprint increases by about 8.5%. Memory guards remain enabled.
Elapsed time and cycles were collected under uncontrolled shared host load; these results
make no latency claim.

Both production compilers use Apple Clang 21 on an Apple M5 Max, with no PGO:

```sh
env -u VFLAGS -u VEXE -u LLVM_PROFILE_FILE VJOBS=4 SOURCE_DATE_EPOCH=0 \
  V_MACOS_V3_NO_FALLBACK=1 \
  ./v -prod -cc clang -gc none -prealloc -nocache -o /path/to/compiler cmd/v
```

The baseline and candidate binary SHA-256 values are respectively:

```text
74b0eee637cd6b5f696751e92f85d1bf3dec615f31fc9aa7965548313f90c360
383b8778a56c10e26993e2bc2f5f764422f3264ec77fb42f08eb9568cd506760
```

Generated C differs only by removing one unused tuple forward declaration and definition
(`multi_return_i64_i64_Array`, 144 bytes). Executable code is identical. Both outputs pass
Clang syntax checking, and Vox builds and links with the candidate compiler.

Evidence is retained at `/Users/alex/.cache/v-vox-instructions-20261029/`.
`pairs-auto/` contains all raw samples, commands, effective environment, compiler hashes,
role outputs, and statistics. `vox-parsed-manifest.json` preserves every parsed file's hash
and physical line count. The private Vox source snapshot stays outside the public change.

## Serial and self-compilation checks

Two separate ten-pair comparisons use `VJOBS=1`, with one excluded warmup per role.

| Workload | Baseline instructions | Candidate instructions | Reduction |
| --- | ---: | ---: | ---: |
| Vox | 88,645,541,044.5 | 85,098,497,829.0 | 4.0014% |
| Fixed compiler self-build | 46,385,502,789.5 | 46,377,622,338.5 | 0.0170% |

The Vox comparison parses the same 637,670 lines and produces the same role-specific C hashes
as the automatic-job comparison. All ten pairs improve. Median resident memory is similar;
physical footprint increases from 2,734.68 MiB to 3,168.36 MiB.

The self-build parses **551,861 physical lines in 496 V files**, no V header files, from
an unchanged archive of the baseline compiler sources. Both outputs are byte-identical:

```text
1f2da5e248f5d23b41a4edf73ef4acd035a12637c72edec1d4f110c690832d61
```

Self-build instruction medians differ by only 0.017%, with mixed pair results: six pairs
increase and the median paired difference is `+7,939,614` instructions (about 0.017%).
This establishes no meaningful self-build efficiency change. Median resident memory is
1,023.36 MiB before and 1,023.34 MiB after. Raw runs and matching source inventories are retained in
`pairs-serial/` and `pairs-self/` beside the automatic-job evidence.

Serial Vox medians are 139,014.76 and 133,452.25 instructions per parsed line;
the median paired instruction difference is `-3,586,635,340.5`. Elapsed medians are
20.105 and 17.360 seconds, and cycle medians are 46.173 and 43.982 billion.
Self-build medians are 84,052.87 and 84,038.59 instructions per parsed line,
7.07 and 7.33 seconds, and 22.281 and 22.831 billion cycles. Physical footprint is
1,010.94 and 1,010.95 MiB. These additional timing and cycle figures share the same
host-load caveat.

The common self-build C links into a native compiler, which regenerates byte-identical C
for the same fixture and flags. The fixture executable is restored after validation.
