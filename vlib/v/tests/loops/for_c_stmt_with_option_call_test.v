import rand
import math { abs }

const nmax = 20

fn ana(n int) f64 {
	return 1.0 // haven't started
}

fn avg(n int) f64 {
	tests := 1e4
	mut sum := 0
	for t := 0; t < tests; t++ {
		mut v := []bool{len: nmax, init: false}
		for x := 0; !v[x]; x = rand.intn(n) or { 0 } {
			v[x] = true
			sum++
		}
	}
	return f64(sum) / tests
}

fn test_for_c_stmt_with_option_call() {
	println(' N   average   analytical   (error)')
	println('=== ========= ============ =========')
	for n in 1 .. nmax + 1 {
		a := avg(n)
		b := ana(n)
		println('${n:3} ${a:9.4f} ${b:12.4f} (${(abs(a - b) / b * 100):6.2f}%)')
	}
	assert true
}
