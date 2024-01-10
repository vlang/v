import os
import rand

fn main() {
	mut arg := '31'
	if os.args.len != 2 {
		println('Usage: rule110 [<n>]')
		println('Using default `n` value: 31')
	} else {
		arg = os.args[1]
	}

	mut n := arg.int()
	if n > 200 || n < 3 {
		eprintln('`n` must be between 3 and 200!')
		exit(1)
	}

	print('\n')
	title := ' Rule 110 V Implementation '
	title_len := title.len
	if n > title_len {
		for _ in 0 .. (n - title_len) / 2 {
			print('=')
		}
		print(title)
		for _ in 0 .. (n - title_len) / 2 {
			print('=')
		}
	} else {
		println(title[1..(title_len - 1)])
	}

	mut generation_bin := []int{len: n}
	for i in 0 .. n {
		generation_bin[i] = rand.intn(2) or { 0 }
	}
	print('\n')

	// println('Random generated first automaton content: $generation_bin')
	for _ in 0 .. n {
		print_generation(generation_bin)
		next_generation(mut generation_bin)
	}
}

fn print_generation(arr []int) {
	symbols := [' ', '*']!
	for i in 0 .. arr.len {
		print(symbols[arr[i]])
	}
	print('\n')
}

fn next_generation(mut gen []int) {
	mut arr := gen.clone()
	mut prev := 0
	mut curr := 0
	mut next := 0
	for i in 0 .. arr.len {
		if (i - 1) % gen.len < 0 {
			prev = gen[gen.len - 1]
		} else {
			prev = gen[(i - 1) % gen.len]
		}
		curr = gen[i]
		next = gen[(i + 1) % gen.len]

		if prev == 1 {
			if curr == 1 {
				if next == 1 { // 111
					arr[i] = 0
				} else { // 110
					arr[i] = 1
				}
			} else {
				if next == 1 { // 101
					arr[i] = 1
				} else { // 100
					arr[i] = 0
				}
			}
		} else {
			if curr == 1 {
				if next == 1 { // 011
					arr[i] = 1
				} else { // 010
					arr[i] = 1
				}
			} else {
				if next == 1 { // 001
					arr[i] = 1
				} else { // 000
					arr[i] = 0
				}
			}
		}
	}
	gen = arr.clone()
}
