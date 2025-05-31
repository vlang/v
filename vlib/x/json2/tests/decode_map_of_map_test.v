import x.json2

struct CrossVerifyResult {
	confusion_matrix_map map[string]map[string]f64
}

fn test_main() {
	x := json2.decode[CrossVerifyResult]('') or {
		assert err.msg().contains('invalid token')
		return
	}
	assert false
}
