module builtin

pub enum BTest_enum {
	v0 = 0
	v1
	v2
	v3
	v_t
}

pub fn (x BTest_enum) tst_f() string {
	match x {
		.v0 { return 'v0' }
		.v1 { return 'v1' }
		.v2 { return 'v2' }
		else { return 'v_t' }
	}
}
