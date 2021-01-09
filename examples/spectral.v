/*
https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/spectralnorm.html
Added: Pradeep Verghese
Benchmarks:
Used v -prod spectral.v
Command: time ./spectral 5500
Output: 1.274224153

Time: 11.67s user 0.02s system 99% cpu 11.721 total
*/
module main

import math
import os
import strconv

fn evala(i int, j int) int {
	return ((i + j) * (i + j + 1) / 2 + i + 1)
}

fn times(mut v []f64, u []f64) {
	for i in 0 .. v.len {
		mut a := f64(0)
		for j in 0 .. u.len {
			a += u[j] / f64(evala(i, j))
		}
		v[i] = a
	}
}

fn times_trans(mut v []f64, u []f64) {
	for i in 0 .. v.len {
		mut a := f64(0)
		for j in 0 .. u.len {
			a += u[j] / f64(evala(j, i))
		}
		v[i] = a
	}
}

fn a_times_transp(mut v []f64, u []f64) {
	mut x := []f64{len: u.len, init: 0}
	times(mut x, u)
	times_trans(mut v, x)
}

fn main() {
	mut n := 0
	if os.args.len == 2 {
		n = strconv.atoi(os.args[1]) or { 0 }
	} else {
		n = 0
	}
	mut u := []f64{len: n, init: 1}
	mut v := []f64{len: n, init: 1}
	for _ in 0 .. 10 {
		a_times_transp(mut v, u)
		a_times_transp(mut u, v)
	}
	mut vbv := f64(0)
	mut vv := f64(0)
	for i in 0 .. n {
		vbv += u[i] * v[i]
		vv += v[i] * v[i]
	}
	ans := math.sqrt(vbv / vv)
	println('${ans:0.9f}')
}
