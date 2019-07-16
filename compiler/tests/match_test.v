fn test_switch() {
	a := 3 
	mut b := 0 
	match a {
	2 => println('two') 
	3 => println('three') 
	     b = 3 
	4 => println('4') 
	} 
	assert b == 3 
} 
