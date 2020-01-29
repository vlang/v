import rand
import time
import builtin.hashmap

fn hashmap_set_bench(arr []string, repeat int) {
	start_time := time.ticks()
	for _ in 0..repeat {
		mut b := hashmap.new_hashmap()
		for x in arr {
			b.set(x, 1)
		}
	}
	end_time := time.ticks() - start_time
	println("* hashmap_set: ${end_time} ms")
}

fn map_set_bench(arr []string, repeat int) {
	start_time := time.ticks()
	for _ in 0..repeat {
		mut b := map[string]int
		for x in arr {
			b[x] = 1
		}
	}
	end_time := time.ticks() - start_time
	println("* map_set: ${end_time} ms")
}

fn hashmap_get_bench(arr []string, repeat int) {
	mut b := hashmap.new_hashmap()
	for x in arr {
		b.set(x, 1)
	}
	start_time := time.ticks()
	for _ in 0..repeat {
		for x in arr {
			b.get(x)
		}
	}
	end_time := time.ticks() - start_time
	println("* hashmap_get: ${end_time} ms")
}

fn map_get_bench(arr []string, repeat int) {
	mut b := map[string]int
	for x in arr {
		b[x] = 1
	}
	start_time := time.ticks()
	for _ in 0..repeat {
		for x in arr {
			b[x]
		}
	}
	end_time := time.ticks() - start_time
	println("* map_get: ${end_time} ms")
}

fn benchmark_many_keys() {
	key_len := 30
	repeat := 1
	for i := 2048; i <= 10000000; i = i * 2 {
		mut arr := []string
		for _ in 0..i {
			mut buf := []byte
			for j in 0..key_len {
				buf << byte(rand.next(int(`z`) - int(`a`)) + `a`)
			}
			s := string(buf)
			arr << s
		}
		println("$arr.len keys of length $key_len")
		// Uncomment the benchmark you would like to benchmark
		// Run one or two at a time while memory leaks is a thing
		hashmap_get_bench(arr, repeat)
		map_get_bench(arr, repeat)
		// hashmap_set_bench(arr, repeat)
		// map_set_bench(arr, repeat)
		println('')
	}
}

fn benchmark_few_keys() {
	key_len := 30
	repeat := 10000
	println("Benchmarks are repeated $repeat times")
	for i := 16; i <= 2048; i = i * 2 {
		mut arr := []string
		for _ in 0..i {
			mut buf := []byte
			for j in 0..key_len {
				buf << byte(rand.next(int(`z`) - int(`a`)) + `a`)
			}
			s := string(buf)
			arr << s
		}
		println("$arr.len keys of length $key_len")
		// Uncomment the benchmark you would like to benchmark
		// Run one or two  at a time while memory leaks is a thing
		hashmap_get_bench(arr, repeat)
		map_get_bench(arr, repeat)
		// hashmap_set_bench(arr, repeat)
		// map_set_bench(arr, repeat)
		println('')
	}
}

fn main() {
	// Uncomment below to benchmark on many keys
	// benchmark_many_keys()
	benchmark_few_keys()
}