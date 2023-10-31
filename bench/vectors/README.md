# Comparing C# and V Boids Algorithm Implementations

## Running the C# program:

```
dotnet run
```

Creating a release version of the C# program:

```
dotnet publish -c Release -r ubuntu.20.04-x64
```

The generated executable will be in
`/v/vnew/bench/vectors/bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors`
Its size is ~64MB . After stripping, the executable shrinks to just 11MB,
but unfortunately it also stops running after stripping :-| .

## Compiling and running the V program:

```
v crun vectors.v
```

... produces and runs a `vectors` executable which is ~1.3MB in size.

Compiling and running the V program, compiled with -prod:

```
v -prod crun vectors.v
```

... produces and runs a `vectors` executable which is ~176KB in size.
After stripping, the executable shrinks to 157KB. It can still run after
stripping.

Note: using `crun` will make sure that the compilation will happen just
once at the start, and then the executable will be just reused by the
subsequent commands with identical options. It will also ensure that
the compiled executable will not be removed, unlike `run` .

## Some measurements and comparisons

Note: the following was done on Intel(R) Core(TM) i3-3225, 16GB RAM:

```
#0 13:41:35 ᛋ master /v/vnew/bench/vectors❱rm -rf vectors
#0 13:41:49 ᛋ master /v/vnew/bench/vectors❱
#0 13:41:49 ᛋ master /v/vnew/bench/vectors❱v -o vectors_development vectors.v
#0 13:42:14 ᛋ master /v/vnew/bench/vectors❱v -o vectors_production -prod vectors.v
#0 13:42:28 ᛋ master /v/vnew/bench/vectors❱
#0 13:42:29 ᛋ master /v/vnew/bench/vectors❱hyperfine ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors ./vectors_development ./vectors_production
Benchmark 1: ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors
  Time (mean ± σ):     347.4 ms ±   7.4 ms    [User: 334.4 ms, System: 13.0 ms]
  Range (min … max):   340.2 ms … 361.7 ms    10 runs

Benchmark 2: ./vectors_development
  Time (mean ± σ):     882.6 ms ±  14.0 ms    [User: 880.3 ms, System: 2.3 ms]
  Range (min … max):   862.4 ms … 912.9 ms    10 runs

Benchmark 3: ./vectors_production
  Time (mean ± σ):     217.9 ms ±   9.4 ms    [User: 216.8 ms, System: 0.9 ms]
  Range (min … max):   206.4 ms … 241.3 ms    12 runs

Summary
  ./vectors_production ran
    1.59 ± 0.08 times faster than ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors
    4.05 ± 0.19 times faster than ./vectors_development
#0 13:43:00 ᛋ master /v/vnew/bench/vectors❱
#0 13:45:07 ᛋ master /v/vnew/bench/vectors❱ls -nlarS ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors ./vectors_development ./vectors_production
-rwxrwxr-x 1 1000 1000   179384 Sep  6 13:42 ./vectors_production
-rwxrwxr-x 1 1000 1000  1320764 Sep  6 13:42 ./vectors_development
-rwxr-xr-x 1 1000 1000 66732821 Sep  6 13:40 ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors
#0 13:45:12 ᛋ master /v/vnew/bench/vectors❱
#0 13:53:12 ᛋ master /v/vnew/bench/vectors❱alias xtime='/usr/bin/time -f "CPU: %Us\tReal: %es\tElapsed: %E\tRAM: %MKB\t%C"'
#0 13:53:42 ᛋ master /v/vnew/bench/vectors❱xtime ./vectors_development
5.0498380931718074e+07 - 5.0504723697762154e+07 - 5.040198063489048e+07
0.0 - 0.0 - 0.0
CPU: 0.87s      Real: 0.87s     Elapsed: 0:00.87        RAM: 4404KB     ./vectors_development
#0 13:53:52 ᛋ master /v/vnew/bench/vectors❱xtime ./vectors_production
4.971971434731853e+07 - 4.973120986372047e+07 - 5.030988639116867e+07
0.0 - 0.0 - 0.0
CPU: 0.20s      Real: 0.20s     Elapsed: 0:00.20        RAM: 3228KB     ./vectors_production
#0 13:53:58 ᛋ master /v/vnew/bench/vectors❱xtime ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors
(49627678.97075553, 50302418.6269631, 49705582.70645027)
(0, 0, 0)
CPU: 0.33s      Real: 0.34s     Elapsed: 0:00.34        RAM: 30544KB    ./bin/Release/net7.0/ubuntu.20.04-x64/publish/vectors
#0 13:54:02 ᛋ master /v/vnew/bench/vectors❱
#0 14:01:33 ᛋ master /v/vnew/bench/vectors❱
#0 14:01:35 ᛋ master /v/vnew/bench/vectors❱xtime v vectors.v
CPU: 0.41s      Real: 0.36s     Elapsed: 0:00.36        RAM: 59412KB    v vectors.v
#0 14:01:41 ᛋ master /v/vnew/bench/vectors❱
#0 14:01:42 ᛋ master /v/vnew/bench/vectors❱xtime v -prod vectors.v
CPU: 4.97s      Real: 5.11s     Elapsed: 0:05.11        RAM: 80732KB    v -prod vectors.v
#0 14:01:48 ᛋ master /v/vnew/bench/vectors❱
#0 14:01:50 ᛋ master /v/vnew/bench/vectors❱xtime dotnet publish -c Release -r ubuntu.20.04-x64
MSBuild version 17.7.1+971bf70db for .NET
  Determining projects to restore...
  All projects are up-to-date for restore.
  vectors -> /v/vnew/bench/vectors/bin/Release/net7.0/ubuntu.20.04-x64/vectors.dll
  vectors -> /v/vnew/bench/vectors/bin/Release/net7.0/ubuntu.20.04-x64/publish/
CPU: 2.34s      Real: 2.64s     Elapsed: 0:02.64        RAM: 159816KB   dotnet publish -c Release -r ubuntu.20.04-x64
#0 14:01:56 ᛋ master /v/vnew/bench/vectors❱
```
