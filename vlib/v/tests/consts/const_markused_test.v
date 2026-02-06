import my_utils

fn test_main() {
	v := f64(1.0 / 8)
	assert my_utils.my_round(v, 2) == '0.13'
}
