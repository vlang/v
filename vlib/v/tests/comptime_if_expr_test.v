const (
	version              = 123
	disable_opt_features = true
)

// NB: the `unknown_fn()` calls are here on purpose, to make sure that anything
// that doesn't match a compile-time condition is not even parsed.
fn test_ct_expressions() {
	foo := version
	bar := foo
	$if bar == 123 {
		assert true
	} $else {
		unknown_fn()
	}

	$if bar != 123 {
		unknown_fn()
	} $else $if bar != 124 {
		assert true
	} $else {
		unknown_fn()
	}

	$if !disable_opt_features {
		unknown_fn()
	} $else {
		assert true
	}
}
