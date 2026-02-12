// Test for autofree with string interpolation inside or blocks
// Issue: autofree was generating duplicate free statements after return in or blocks
import compress.gzip

fn test_autofree_string_interpolation_in_or_with_return() {
	data := 'test data'.repeat(100).bytes()

	mut result := ''
	compressed := gzip.compress(data) or {
		variable := 'compression'
		result = 'Error during ${variable}'
		return
	}

	assert compressed.len > 0
	assert result == ''
}

fn test_autofree_string_interpolation_in_or_with_break() {
	for i in 0 .. 1 {
		data := 'test'.bytes()

		_ := gzip.compress(data) or {
			msg := 'failed'
			eprintln('Compression ${msg}')
			break
		}
	}
	assert true
}

fn test_autofree_string_interpolation_in_or_with_continue() {
	mut count := 0
	for i in 0 .. 2 {
		data := 'test'.bytes()

		_ := gzip.compress(data) or {
			reason := 'error'
			eprintln('Got ${reason}')
			count++
			continue
		}
		count += 10
	}
	assert count == 20
}
