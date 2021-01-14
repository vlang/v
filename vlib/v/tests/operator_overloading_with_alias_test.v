type Custom = string

type CustomInt = int

fn (c Custom) * (c1 Custom) Custom {
	return Custom(c) + Custom(c1)
}

fn (c CustomInt) < (c1 CustomInt) bool {
	return CustomInt(c) > CustomInt(c1)
}

fn test_operator_overloading_alias() {
	mut c := Custom('custom')
	assert c * Custom(' operation') == Custom('custom operation')
	assert (c * Custom(' operation')).str() == 'custom operation'
	c *= Custom(' type')
	assert c == Custom('custom type')
	assert c.str() == 'custom type'
	c += Custom(' str')
	assert c.str() == 'custom type str'
	assert c >= Custom('abs')
	assert c <= Custom('zebra')
	assert c != Custom('custom')
	assert c < Custom('zebra')
	assert c > Custom('abs')
	/// //// ///
	mut custom_int := CustomInt(12)
	assert custom_int == CustomInt(12)
	assert custom_int + CustomInt(12) == 24
	assert custom_int < CustomInt(24)
	custom_int += CustomInt(12)
	assert custom_int == CustomInt(24)
	custom_int *= CustomInt(2)
	assert custom_int == CustomInt(48)

}
