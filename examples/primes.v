fn is_prime(n int) bool {
	if n <= 1 {
		return false
	}
	for i := 2; i * i <= n; i++ {
		if n % i == 0 {
			return false
		}
	}
	return true
}

fn main() {
	how_many := arguments()[1] or { '10' }.int()
	mut count := 0
	mut num := 2
	for count < how_many {
		if is_prime(num) {
			println(num)
			count++
		}
		num++
	}
}
