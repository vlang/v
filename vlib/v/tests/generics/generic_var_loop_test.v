import math

fn t[T](a1 []int, a2 []int) T {
	mut a := 0 * a1[0] - a2[0]
	a = math.max(a, 10)
	mut t := T{}
	for i in a .. 20 {
		t += i
		for j in a .. 20 {
			t += j
		}
	}
	return t
}

fn test_main() {
	assert t[int]([1, 2, 3], [4, 5, 6]) == 1595
}
