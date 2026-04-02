# V Benchmarks

All benchmarks compiled with `v -prod` on Apple M5, 16 GB RAM, macOS (arm64).
V version: 0.5.1.

## GC: Boehm vs VGC

Compares Boehm GC (`-gc boehm`) against V's built-in concurrent tri-color
mark-and-sweep (`-gc vgc`).
5 iterations per test, median reported.

```
v run bench/bench_gc.v
```

```
  test                                             boehm       vgc     ratio
  ———————————————————————————————————————————— ————————— ————————— —————————
  small allocs (1000000x string)                   39 ms     48 ms    1.23x
  tree build+walk (depth=18, 10x)                  48 ms    118 ms    2.46x
  array grow (100x 100000 pushes)                   9 ms     26 ms    2.89x
  map insert (20x 10k entries)                     20 ms     27 ms    1.35x
  mixed workload (50 rounds)                       10 ms     16 ms    1.60x

  heap usage:
    boehm: 29856 KB allocated, 29296 KB free
    vgc:   131072 KB allocated, 0 KB free
```

Boehm is still 1.2x-2.9x faster across these workloads and uses ~4x less heap.

## Closures

Measures closure creation, invocation, multi-threaded creation, and memory overhead.

```
v -prod -o /tmp/bench_closure bench/bench_closure.v && /tmp/bench_closure
```

```
| Test Name                 | Iterations | Time(ms) | Ops/sec      |
|---------------------------|------------|----------|--------------|
| Normal Function Call      |  100000000 |        0 |  +inf Mop/s  |
| Small Closure Creation    |   10000000 |      188 | 53.19 Mop/s  |
| Medium Closure Creation   |   10000000 |      376 | 26.60 Mop/s  |
| Large Closure Creation    |    1000000 |      121 |  8.26 Mop/s  |
| Small Closure Call        |  100000000 |      136 | 735.29 Mop/s |
| Medium Closure Call       |  100000000 |      133 | 751.88 Mop/s |
| Large Closure Call        |   10000000 |       16 | 625.00 Mop/s |
| Multi-threaded Creation   |    1000000 |       95 | 10.53 Mop/s  |
```

Memory: ~69 bytes per closure (medium, 4 captured vars). Closure calls are ~625-750 Mop/s.

## String Deduplication

Compares four deduplication strategies on 10,000 strings with ~30% duplicates.

```
v -prod -o /tmp/bench_string_dedup bench/bench_string_dedup.v && /tmp/bench_string_dedup
```

```
Method 1 (basic array)          33 ms   7000 unique
Method 2 (pre-allocated array)  27 ms   7000 unique
Method 3 (map)                   0 ms   7000 unique
Method 4 (set)                   0 ms   7000 unique
```

Maps and sets are orders of magnitude faster than linear array search for deduplication.

## Vectors (Boids Simulation)

N-body boids simulation with 10,000 entities: cohesion, separation, and alignment.

```
v -prod -o /tmp/bench_vectors bench/vectors/vectors.v && /tmp/bench_vectors
```

```
~50 ms per run (after warmup)
```

## Crypto: ECDSA

Key generation, signing, and verification (1,000 iterations each).

```
v -prod -o /tmp/bench_ecdsa bench/crypto/ecdsa/ecdsa.v && /tmp/bench_ecdsa
```

```
Average key generation time:   9 µs
Average sign time:            11 µs
Average verify time:          30 µs
```

## SOA Structs (V2 cleanc only)

Compares Array-of-Structs vs Struct-of-Arrays memory layout for a 16-field
particle system (500k particles). Uses V2's `@[soa]` attribute which auto-generates
separate contiguous arrays per field for better cache utilization.

```
./cmd/v2/v2 -prod -backend cleanc bench/bench_soa_structs.v -o bench/bench_soa_structs
./bench/bench_soa_structs
```

```
build particles
  aos: 20 ms    soa push: 125 ms    soa indexed: 37 ms

sum x only
  aos: 19 ms    soa: 12 ms    speedup: 1.58x

sum x/y/z/life (4 of 16 fields)
  aos: 14 ms    soa: 10 ms    speedup: 1.40x

sum all 16 fields
  aos: 11 ms    soa: 16 ms    speedup: 0.69x

integrate position/velocity/life
  aos: 10 ms    soa: 13 ms    speedup: 0.77x
```

SOA is 1.4x-1.6x faster for partial field access (fewer cache lines touched).
When all fields are accessed or mutated, AOS wins due to less pointer indirection.
