import time

// Collatz inputs must be positive, and every intermediate value must fit in u32.
fn next_collatz_value(current u32) !u32 {
	if current % 2 == 0 {
		return current / 2
	}
	if current > (max_u32 - 1) / 3 {
		return error('Collatz chain exceeds u32')
	}
	return 3 * current + 1
}

struct MasterCollatz {
mut:
	chains map[u32][]u32
	steps  map[u32]u32
}

fn MasterCollatz.new() MasterCollatz {
	return MasterCollatz{
		chains: map[u32][]u32{}
		steps:  map[u32]u32{}
	}
}

fn (mut m MasterCollatz) get_collatz_chain(num u32) ![]u32 {
	if num == 0 {
		return error('Collatz chains require a positive input')
	}
	// If cached, return a clone of it
	if num in m.chains {
		return m.chains[num].clone()
	}

	// Otherwise compute it
	mut output := []u32{}
	mut current := num
	output << current

	for current != 1 {
		current = next_collatz_value(current)!
		output << current
	}

	m.chains[num] = output.clone()
	m.steps[num] = u32(output.len)
	return output
}

fn (mut m MasterCollatz) compute_fast_chain(num u32) ![]u32 {
	if num == 0 {
		return error('Collatz chains require a positive input')
	}
	if num in m.chains {
		return m.chains[num].clone()
	}

	mut output := []u32{}
	mut current := num
	output << num

	for current != 1 {
		if current in m.chains {
			chain := m.chains[current]
			// current is already the last value in output.
			output << chain[1..]
			break
		}
		current = next_collatz_value(current)!
		output << current
	}
	m.chains[num] = output.clone()
	m.steps[num] = u32(output.len)
	return output
}

struct MasterPell {
mut:
	series map[u32]u32
}

fn MasterPell.new() MasterPell {
	mut new_series := map[u32]u32{}
	new_series[0] = 0
	new_series[1] = 1
	return MasterPell{
		series: new_series
	}
}

fn (mut p MasterPell) compute(index u32) !u32 {
	// P(26) is the last Pell number that fits in u32.
	if index > 26 {
		return error('Pell index must be at most 26 for u32')
	}
	if index in p.series {
		return p.series[index]
	}

	output := 2 * p.compute(index - 1)! + p.compute(index - 2)!
	p.series[index] = output
	return output
}

fn benchmark_collatz_standard(mut m MasterCollatz, value u32) !f32 {
	sw := time.new_stopwatch()
	m.get_collatz_chain(value)!
	return f32(sw.elapsed().milliseconds())
}

fn benchmark_collatz_fast(mut m MasterCollatz, value u32) !f32 {
	sw := time.new_stopwatch()
	m.compute_fast_chain(value)!
	return f32(sw.elapsed().milliseconds())
}

fn run_example() ! {
	mut my_collatz := MasterCollatz.new()
	mut my_pell := MasterPell.new()

	println('Collatz chain of 5: ${my_collatz.get_collatz_chain(5)!}.')
	println('Pell 0: ${my_pell.compute(0)!}.')
	println('Pell 5: ${my_pell.compute(5)!}.')
	println('Pell 9: ${my_pell.compute(9)!}.')

	println('Collatz chain of 100: ${my_collatz.compute_fast_chain(100)!}.')

	println('Benchmarks #1:')

	mut bench_collatz_1 := MasterCollatz.new()
	println('CollatzChain(1000) by standard method: ${benchmark_collatz_standard(mut bench_collatz_1, 1000)!}ms.')

	mut bench_collatz_2 := MasterCollatz.new()
	println('CollatzChain(1000) by fast method: ${benchmark_collatz_fast(mut bench_collatz_2, 1000)!}ms.')
}

fn main() {
	run_example() or { panic(err) }
}
