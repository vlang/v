fn test_vweb_templates() {
	str := $html('./templates/index.html')
	println(str)
	x := true
	assert x == true
}