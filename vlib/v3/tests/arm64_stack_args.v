fn sum9(a int, b int, c int, d int, e int, f int, g int, h int, i int) int {
	return a + b + c + d + e + f + g + h + i
}

fn main() {
	if sum9(1, 2, 3, 4, 5, 6, 7, 8, 9) == 45 {
		C.putchar(111)
		C.putchar(107)
		C.putchar(10)
	} else {
		C.putchar(98)
		C.putchar(97)
		C.putchar(100)
		C.putchar(10)
	}
}
