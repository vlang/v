type Custom = string 

fn (c Custom) + (c1 Custom) Custom {
	return Custom(c + c1)
}

fn test_operator_overloading_alias() {
	mut c := Custom('custom')
	assert c + Custom(' operation') == Custom('custom operation')
	assert (c + Custom(' operation')).str() == 'custom operation'
	c += Custom(' type')
	assert c == Custom('custom type')
	assert c.str() == 'custom type'

}
