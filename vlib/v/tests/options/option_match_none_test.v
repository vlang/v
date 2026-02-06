fn test_main() {
	a := match 1 {
		0 { none }
		else { 1 }
	}
	assert '${a}' == 'Option(1)'

	b := match 2 {
		0 { none }
		2 { none }
		else { 1 }
	}
	assert '${b}' == 'Option(none)'

	c := match 2 {
		0 { 1 }
		2 { 2 }
		else { none }
	}
	assert '${c}' == 'Option(2)'
}
