struct Symbol {
	name string
	file string
}

// the expected values come from the same `$if` forms with no local named after a
// flag, so the assertions hold on every host OS
fn expected_pick() string {
	return $if windows { 'win.c.v' } $else { 'nix.c.v' }
}

fn expected_chain() string {
	mut f := 'none'
	$if windows {
		f = 'win.c.v'
	} $else $if macos {
		f = 'macos.c.v'
	} $else {
		f = 'nix.c.v'
	}
	return f
}

fn test_flag_expr_with_struct_values_and_local_named_windows() {
	win := Symbol{'win', 'win.c.v'}
	nix := Symbol{'nix', 'nix.c.v'}
	windows := Symbol{'local', 'local.c.v'}
	own := $if windows { win } $else { nix }
	assert windows.name == 'local'
	assert own.file == expected_pick()
}

fn test_flag_expr_with_local_named_macos() {
	win := Symbol{'win', 'win.c.v'}
	nix := Symbol{'nix', 'nix.c.v'}
	want := $if macos { win.file } $else { nix.file }
	macos := 42
	own := $if macos { win } $else { nix }
	assert macos == 42
	assert own.file == want
}

fn test_flag_chain_statement_with_local_named_windows() {
	windows := true
	mut own_name := 'none'
	$if windows {
		own_name = 'win.c.v'
	} $else $if macos {
		own_name = 'macos.c.v'
	} $else {
		own_name = 'nix.c.v'
	}
	assert windows
	assert own_name == expected_chain()
}

fn test_flag_chain_statement_with_local_named_linux() {
	linux := 'local'
	mut own_name := 'none'
	$if windows {
		own_name = 'win.c.v'
	} $else $if linux {
		own_name = 'linux.c.v'
	} $else {
		own_name = 'nix.c.v'
	}
	assert linux == 'local'
	mut want := 'none'
	$if windows {
		want = 'win.c.v'
	} $else $if linux {
		want = 'linux.c.v'
	} $else {
		want = 'nix.c.v'
	}
	assert own_name == want
}
