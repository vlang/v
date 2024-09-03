struct Abc {
	version string
	name    string
}

fn abc() !Abc {
	return Abc{
		version: 'abc'
		name:    'xyz'
	}
}

// Note: version depends on a field in manifest, even though it is declared before it
pub const version = manifest.version

pub const manifest = abc() or { panic(err) }

// Note: name depends on a field in manifest too, but it is declared after it
pub const name = manifest.name

fn test_order_of_const_initialisation_should_take_into_account_selector_expressions() {
	println(version)
	println(name)
	assert version == 'abc'
	assert name == 'xyz'
}
