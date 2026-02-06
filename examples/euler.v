// This example shows 2 solutions for https://projecteuler.net/problem=1 :
fn gauss_sum(n int) fn (int) int {
	return fn [n] (m int) int {
		return m * ((n - 1) / m) * ((n - 1) / m + 1) / 2
	}
}

gs := gauss_sum(1000)
println('O(1) arithmetic progression sum: ${gs(3) + gs(5) - gs(15)}')

// A brute force solution, by checking every n in the range:
mut sum := 0
for n in 1 .. 1000 {
	if n % 3 == 0 || n % 5 == 0 {
		sum += n
	}
}
println('O(n) brute force calculated sum: ${sum}')
