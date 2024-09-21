fn template() string {
	one := 1
	two := 2
	return $tmpl('tmpl/dot_var.txt')
}

fn test_tmpl_with_dot_var() {
	assert template() == '.1
.2
'
}
