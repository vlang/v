import net.http

fn test_main() {
	mut count := 0
	$for method_val in http.Method.values {
		println(method_val.name)
		count++
	}
	assert count > 0
}
