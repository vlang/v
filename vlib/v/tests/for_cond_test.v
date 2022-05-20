type TokenValue = rune | u64

fn test_for_cond() {
	val := `+`
	for (val in [TokenValue(`+`), TokenValue(`-`)]) {
		println('ok')
		break
	}
	assert true
}
