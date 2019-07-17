fn test_match() {
	a := 3 
	mut b := 0 
	match a {
	2 => println('two') 
	3 => println('three') 
	     b = 3 
	4 => println('four') 
	} 
	assert b == 3 
} 
