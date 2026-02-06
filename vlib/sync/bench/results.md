# Channel Benchmark Results

This documents lists several benchmark results for different platforms in order to
identify performance regressions and improvements.

The are measured using the command

```
> channel_bench_* <nsend> <nrec> <buflen> <nobj>

nsend ... number of threads that push objects into the channel
nrec .... number of threads that pop objects from the channel
buflen .. length of channel buffer queue - `0` means unbuffered channel
nobj .... number of objects to pass thru the channel
```

## AMD Ryzen 7 3800X, Ubuntu-20.04 x86_64

10000000 Objects transfered, results in Objects/µs

| nsend | nrec | buflen | **V (gcc -O2)** | **V (clang)** | **V (tcc)** | **Go (golang)** | **Go (gccgo -O2)** |
| :---: | :---:| :---:  |      :---:      |    :---:      |    :---:    |     :---:      |      :---:         |
|   1   |   1  |    0   |      1.97       |    1.63       |    2.08     |      4.65      |       0.56         |
|   1   |   1  |   100  |      3.05       |    2.29       |    1.93     |     18.90      |       6.08         |
|   4   |   4  |    0   |      0.87       |    0.90       |    0.99     |      1.84      |       0.84         |
|   4   |   4  |   100  |      3.35       |    3.07       |    2.92     |      7.43      |       3.71         |

## AMD Ryzen 7 3800X, Windows 10 2004 x64

| nsend | nrec | buflen | **V (gcc -O2)**  | **V (msvc /O2)** | **V (tcc)** | **Go (golang)** |
| :---: | :---:| :---:  |      :---:       |      :---:       |    :---:    |     :---:       |
|   1   |   1  |    0   |      2.30        |      3.76        |    2.02     |      4.67       |
|   1   |   1  |   100  |      2.96        |      3.12        |    2.26     |     23.31       |
|   4   |   4  |    0   |      0.90        |      1.05        |    0.83     |      1.38       |
|   4   |   4  |   100  |      2.28        |      2.16        |    2.43     |      4.63       |

## Raspberry Pi 3B+, Void Linux musl 32 bit

10000000 Objects transfered, results in Objects/µs

| nsend | nrec | buflen | **V (gcc -O2)** | **Go (golang)** |
| :---: | :---:| :---:  |      :---:      |     :---:      |
|   1   |   1  |    0   |      0.37       |     0.21       |
|   1   |   1  |   100  |      1.03       |     0.74       |
|   4   |   4  |    0   |      0.04       |     0.38       |
|   4   |   4  |   100  |      2.78       |     2.63       |
|   2   |   2  |    0   |      0.05       |     0.38       |
|   2   |   2  |   100  |      1.26       |     0.75       |
