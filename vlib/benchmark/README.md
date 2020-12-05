Example usage of this module:
```v
import benchmark

mut bmark := benchmark.new_benchmark()
// by default the benchmark will be verbose, i.e. it will include timing information
// if you want it to be silent, set bmark.verbose = false
for {
	bmark.step() // call this when you want to advance the benchmark.
	// The timing info in bmark.step_message will be measured starting from the last call to bmark.step
	// ....
	// bmark.fail() // call this if the step failed
	// bmark.step_message(('failed')
	bmark.ok() // call this when the step succeeded
	println(bmark.step_message('ok'))
}
bmark.stop()
// call when you want to finalize the benchmark
println(bmark.total_message('remarks about the benchmark'))
```

benchmark.start() and b.measure() are convenience methods,
intended to be used in combination. Their goal is to make
benchmarking of small snippets of code as *short*, easy to
write, and then to read and analyze the results, as possible.

Example:
```v
import time
import benchmark

mut b := benchmark.start()
// your code section 1 ...
time.sleep_ms(1500)
b.measure('code_1')
// your code section 2 ...
time.sleep_ms(500)
b.measure('code_2')
```

... which will produce on stdout something like this:
```text
SPENT 1500.063 ms in code_1
SPENT  500.061 ms in code_2
```
