// hanoi tower
const (
	num = 7
)

fn main() {
	hanoi(num, 'A', 'B', 'C')
}

fn move(n int, a string, b string) {
	println('Disc ${n} from ${a} to ${b}...')
}

fn hanoi(n int, a string, b string, c string) {
	if n == 1 {
		move(1, a, c)
	} else {
		hanoi(n - 1, a, c, b)
		move(n, a, c)
		hanoi(n - 1, b, a, c)
	}
}
