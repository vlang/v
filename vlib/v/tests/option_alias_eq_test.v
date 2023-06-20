struct MyStruct {
	field int
}

pub type MyOpt = ?MyStruct

fn empty() map[string]MyOpt {
	return {}
}

fn test_empty() {
	expected := {
		'key': ?MyOpt(none)
	}
	assert dump(expected) == {
		'key': ?MyOpt(none)
	}
}
