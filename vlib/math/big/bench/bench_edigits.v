module main

import benchmark
import math
import math.big
import os
import strconv

// Regression benchmark for issue #21319.
// It mirrors the external edigits workload without printing every line,
// so the timing stays focused on `math.big`.
const default_n = 250_001
const one = big.one_int
const ten = big.c10

fn main() {
	mut n := default_n
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { default_n }
	}
	mut clock := benchmark.start()
	k := binary_search(n)
	clock.measure('k=${k}')
	mut p, q := sum_terms(0, k - 1)
	clock.measure('sum_terms p.bit_len=${p.bit_len()} q.bit_len=${q.bit_len()}')
	p += q
	a := ten.pow(u32(n - 1))
	clock.measure('pow 10^${n - 1}')
	answer := p * a / q
	clock.measure('division answer.bit_len=${answer.bit_len()}')
	s := answer.str()
	clock.measure('str digits=${s.len}')
	if s.len != n {
		eprintln('unexpected digit count: got ${s.len}, want ${n}')
		return
	}
	println(last_line(s, n))
}

fn sum_terms(a int, b int) (big.Integer, big.Integer) {
	if b == a + 1 {
		return one, big.integer_from_int(b)
	}
	mid := (a + b) / 2
	p_left, q_left := sum_terms(a, mid)
	p_right, q_right := sum_terms(mid, b)
	return p_left * q_right + p_right, q_left * q_right
}

fn binary_search(n int) int {
	mut a := 0
	mut b := 1
	for !test_k(n, b) {
		a = b
		b *= 2
	}
	for b - a > 1 {
		m := (a + b) / 2
		if test_k(n, m) {
			b = m
		} else {
			a = m
		}
	}
	return b
}

fn test_k(n int, k int) bool {
	if k < 0 {
		return false
	}
	ln_k_factorial := k * (math.log(k) - 1) + 0.5 * math.log(math.tau)
	log_10_k_factorial := ln_k_factorial / math.ln10
	return log_10_k_factorial >= n + 50
}

fn last_line(s string, n int) string {
	chunk_len := if n % 10 == 0 { 10 } else { n % 10 }
	start := n - chunk_len
	mut line := s[start..n]
	for line.len < 10 {
		line += ' '
	}
	return '${line}\t:${n}'
}
