import math { sqrt }
import time

fn get_hypot(a f64, b f64) f64 {
	time.sleep(100 * time.millisecond)
	c := sqrt(a * a + b * b)
	return c
}

fn test_main() {
	mut arr := &[]thread f64{}
	for num in 1 .. 1000 {
		g := go get_hypot(num, num)
		arr << g
	}
	result := arr.wait()
	assert result[0] == 1.4142135623730951
}
