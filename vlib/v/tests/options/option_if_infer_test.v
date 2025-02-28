fn test_main() {
	v := if true { none } else { 1 }
	assert '${v}' == 'Option(none)'
	v2 := if false { 1 } else { none }
	assert '${v2}' == 'Option(none)'
	v3 := if true { 1 } else { none }
	assert '${v3}' == 'Option(1)'
}
