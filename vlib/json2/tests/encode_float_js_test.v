import json2

fn test_js_float_encoding_uses_supported_builtin_formatting() {
	$if js {
		assert json2.encode(f64(1e-6)) == '0.000001'
		assert json2.encode(f64(1e-7)) == '1e-7'
		assert json2.encode(f64(1e20)) == '100000000000000000000'
		assert json2.encode(f64(1e21)) == '1e+21'
		assert json2.encode(f32(1e6)) == '1000000'
		assert json2.encode([f64(1e-6), 1e-7]) == '[0.000001,1e-7]'
	}
}
