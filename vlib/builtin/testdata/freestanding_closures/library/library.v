module main

@[export: 'v_closure_library_probe']
fn library_probe(value i64) i64 {
	callback := fn [value] () i64 { return value * 3 + 1 }
	return callback()
}
