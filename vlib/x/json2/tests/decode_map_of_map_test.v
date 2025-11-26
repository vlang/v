import x.json2

struct CrossVerifyResult {
	confusion_matrix_map map[string]map[string]f64
}

fn test_main() {
	x := json2.decode[CrossVerifyResult]('') or {
		assert err.msg().contains('1:1: Invalid json: empty string')
		return
	}
	assert false
}
