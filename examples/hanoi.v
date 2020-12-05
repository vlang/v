// hanoi tower
const (
	num = 7
)

fn main() {
	hanoi(num, 'A', 'B', 'C')
}

fn move(n int, a string, b string) int {
	println('Disc $n from $a to ${b}...')
	return 0
}

fn hanoi(n int, a string, b string, c string) int {
	if n == 1 {
		move(1, a, c)
	} else {
		hanoi(n - 1, a, c, b)
		move(n, a, c)
		hanoi(n - 1, b, a, c)
	}
	return 0
}
