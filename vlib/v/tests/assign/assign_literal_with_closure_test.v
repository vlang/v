fn sma(period int) fn (f64) f64 {
	mut i := 0
	mut sum := 0.0
	mut storage := []f64{len: 0, cap: period}

	return fn [mut storage, mut sum, mut i, period] (input f64) f64 {
		if storage.len < period {
			sum += input
			storage << input
		}

		sum += input - storage[i]
		storage[i], i = input, (i + 1) % period
		return sum / f64(storage.len)
	}
}

fn test_assign_literal_with_closure() {
	sma3 := sma(3)
	sma5 := sma(5)
	println('x       sma3   sma5')
	for x in [f64(1), 2, 3, 4, 5, 5, 4, 3, 2, 1] {
		println('${x:5.3f}  ${sma3(x):5.3f}  ${sma5(x):5.3f}')
	}
	assert true
}
