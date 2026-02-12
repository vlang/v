### Environment

- CPU: AMD Ryzen 9 9950X3D (16C / 32T)
- RAM: 64 GiB
- OS: EndeavourOS (Linux, kernel 6.18.6-arch1-1)
- Compiler:
  - amd64: v -prod -cc gcc -gc none
  - i386: v -keepc -cc i686-linux-gnu-gcc -prod -m32 -arch i386 -cflags -mmmx -w -gc none

### How to run

```bash
# amd64
v -prod -cc gcc -gc none run benchmarks/atomic_benchmark.v

# i386
v -keepc -cc i686-linux-gnu-gcc -prod -m32 -arch i386 -cflags -mmmx -w -gc none run benchmarks/atomic_benchmark.v
```

### Results (ns/op, 100M iterations)

```
AMD64
=====

Command:
v -prod -cc gcc -gc none run atomic_benchmark.v

u64 store std: 3.788 ns/op (total: 378.783ms, iters: 100000000)
u64 store custom: 3.773 ns/op (total: 377.301ms, iters: 100000000)
u64 load std: 1.078 ns/op (total: 107.848ms, iters: 100000000)
u64 load custom: 1.084 ns/op (total: 108.381ms, iters: 100000000)
u64 add std: 3.601 ns/op (total: 360.067ms, iters: 100000000)
u64 add custom: 3.782 ns/op (total: 378.213ms, iters: 100000000)
u64 swap std (exchange): 3.805 ns/op (total: 380.520ms, iters: 100000000)
u64 swap custom: 3.835 ns/op (total: 383.493ms, iters: 100000000)
u64 cas std: 3.824 ns/op (total: 382.391ms, iters: 100000000)
u64 cas custom: 3.783 ns/op (total: 378.264ms, iters: 100000000)

u32 store std: 3.783 ns/op (total: 378.346ms, iters: 100000000)
u32 store custom: 3.822 ns/op (total: 382.245ms, iters: 100000000)
u32 load std: 1.084 ns/op (total: 108.427ms, iters: 100000000)
u32 load custom: 1.085 ns/op (total: 108.536ms, iters: 100000000)
u32 add std: 3.663 ns/op (total: 366.308ms, iters: 100000000)
u32 add custom: 3.857 ns/op (total: 385.722ms, iters: 100000000)
u32 swap std (exchange): 3.855 ns/op (total: 385.503ms, iters: 100000000)
u32 swap custom: 3.859 ns/op (total: 385.892ms, iters: 100000000)
u32 cas std: 3.87 ns/op (total: 387.025ms, iters: 100000000)
u32 cas custom: 3.837 ns/op (total: 383.680ms, iters: 100000000)

i64 store std (via u64): 3.784 ns/op (total: 378.377ms, iters: 100000000)
i64 store custom: 3.79 ns/op (total: 378.993ms, iters: 100000000)
i64 load std (via u64): 0.935 ns/op (total: 93.519ms, iters: 100000000)
i64 load custom: 0.864 ns/op (total: 86.350ms, iters: 100000000)
i64 add std (via u64): 3.608 ns/op (total: 360.752ms, iters: 100000000)
i64 add custom: 3.843 ns/op (total: 384.319ms, iters: 100000000)
i64 swap std (exchange u64): 3.826 ns/op (total: 382.621ms, iters: 100000000)
i64 swap custom: 3.835 ns/op (total: 383.513ms, iters: 100000000)
i64 cas std (via u64): 3.84 ns/op (total: 383.988ms, iters: 100000000)
i64 cas custom: 3.847 ns/op (total: 384.733ms, iters: 100000000)

i32 store std (via u32): 3.84 ns/op (total: 383.983ms, iters: 100000000)
i32 store custom: 3.841 ns/op (total: 384.068ms, iters: 100000000)
i32 load std (via u32): 1.1 ns/op (total: 109.956ms, iters: 100000000)
i32 load custom: 1.1 ns/op (total: 109.978ms, iters: 100000000)
i32 add std (via u32): 3.659 ns/op (total: 365.907ms, iters: 100000000)
i32 add custom: 3.846 ns/op (total: 384.574ms, iters: 100000000)
i32 swap std (exchange u32): 3.848 ns/op (total: 384.830ms, iters: 100000000)
i32 swap custom: 3.836 ns/op (total: 383.562ms, iters: 100000000)
i32 cas std (via u32): 3.837 ns/op (total: 383.690ms, iters: 100000000)
i32 cas custom: 3.815 ns/op (total: 381.453ms, iters: 100000000)


I386
====

Command:
v -keepc -cc i686-linux-gnu-gcc -prod -m32 -arch i386 -cflags -mmmx -w -gc none run benchmarks/atomic_benchmark.v

u64 store std: 9.575 ns/op (total: 957.485ms, iters: 100000000)
u64 store custom: 7.703 ns/op (total: 770.251ms, iters: 100000000)
u64 load std: 1.769 ns/op (total: 176.860ms, iters: 100000000)
u64 load custom: 1.892 ns/op (total: 189.238ms, iters: 100000000)
u64 add std: 5.544 ns/op (total: 554.431ms, iters: 100000000)
u64 add custom: 5.31 ns/op (total: 530.964ms, iters: 100000000)
u64 swap std: 5.32 ns/op (total: 531.988ms, iters: 100000000)
u64 swap custom: 5.242 ns/op (total: 524.175ms, iters: 100000000)
u64 cas std: 4.948 ns/op (total: 494.824ms, iters: 100000000)
u64 cas custom: 5.268 ns/op (total: 526.833ms, iters: 100000000)

u32 store std: 3.896 ns/op (total: 389.574ms, iters: 100000000)
u32 store custom: 4.067 ns/op (total: 406.712ms, iters: 100000000)
u32 load std: 1.132 ns/op (total: 113.242ms, iters: 100000000)
u32 load custom: 1.135 ns/op (total: 113.523ms, iters: 100000000)
u32 add std: 3.951 ns/op (total: 395.090ms, iters: 100000000)
u32 add custom: 4.141 ns/op (total: 414.139ms, iters: 100000000)
u32 swap std: 4.136 ns/op (total: 413.586ms, iters: 100000000)
u32 swap custom: 4.138 ns/op (total: 413.812ms, iters: 100000000)
u32 cas std: 4.136 ns/op (total: 413.644ms, iters: 100000000)
u32 cas custom: 4.705 ns/op (total: 470.505ms, iters: 100000000)

i64 store std: 9.643 ns/op (total: 964.327ms, iters: 100000000)
i64 store custom: 7.373 ns/op (total: 737.327ms, iters: 100000000)
i64 load std: 1.7 ns/op (total: 169.983ms, iters: 100000000)
i64 load custom: 1.824 ns/op (total: 182.396ms, iters: 100000000)
i64 add std: 5.27 ns/op (total: 526.950ms, iters: 100000000)
i64 add custom: 5.268 ns/op (total: 526.833ms, iters: 100000000)
i64 swap std: 4.917 ns/op (total: 491.690ms, iters: 100000000)
i64 swap custom: 5.095 ns/op (total: 509.546ms, iters: 100000000)
i64 cas std: 4.931 ns/op (total: 493.122ms, iters: 100000000)
i64 cas custom: 5.268 ns/op (total: 526.774ms, iters: 100000000)

i32 store std: 4.018 ns/op (total: 401.776ms, iters: 100000000)
i32 store custom: 4.138 ns/op (total: 413.820ms, iters: 100000000)
i32 load std: 1.132 ns/op (total: 113.185ms, iters: 100000000)
i32 load custom: 1.134 ns/op (total: 113.359ms, iters: 100000000)
i32 add std: 3.949 ns/op (total: 394.853ms, iters: 100000000)
i32 add custom: 4.139 ns/op (total: 413.947ms, iters: 100000000)
i32 swap std: 4.135 ns/op (total: 413.485ms, iters: 100000000)
i32 swap custom: 4.136 ns/op (total: 413.623ms, iters: 100000000)
i32 cas std: 4.137 ns/op (total: 413.702ms, iters: 100000000)
i32 cas custom: 4.704 ns/op (total: 470.402ms, iters: 100000000)
```