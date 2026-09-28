struct Inner {}

fn (i Inner) hidden() int {
	return 42
}

struct Outer {
	Inner
}

type Alias = Outer

fn test_alias_can_call_promoted_private_method_in_its_module() {
	value := Alias(Outer{})
	assert value.hidden() == 42
	p := &value
	pp := &p
	assert pp.hidden() == 42
}
