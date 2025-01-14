# 1 Billion Row Challenge (1BRC)

A solution to the [1 Billion Row Challenge](https://www.morling.dev/blog/one-billion-row-challenge/), 
written in [the V programming language](https://vlang.io/).

Read more about the challenge here: https://www.morling.dev/blog/one-billion-row-challenge/


## Running instructions

Suggested compiler options for performance: 

`v -cc gcc -prod -cflags "-std=c17 -march=native -mtune=native" .`


### Step 1: Create a measurements file

Compile and run `make-samples` to create the sample file.

```
cd make-samples
v -cc gcc -prod -cflags "-std=c17 -march=native -mtune=native" .
./make-samples 1000000000 > ~/measurements.txt
```

NOTE: If you create a billion rows, the file will be about 12GB!

### Step 2: Run (and time) the solution

```
cd solution
v -cc gcc -prod -cflags "-std=c17 -march=native -mtune=native" .
./solution ~/measurements.txt
```

You can time the solution using `v time`:

`v time ./solution ~/measurements.txt`

By default, the solution runs in a single thread. If you want to run
parallel processing, use the `-n` parameter, for example, to run with 
8 threads:

`./solution -n 8 ~/measurements.txt`

On Linux, to run one thread per core, use

`./solution -n $(nproc) ~/measurements.txt`

### Step 3: Improve upon the solution

Make changes that improve the performance and submit them. 
Let's show off what is possible in V!
