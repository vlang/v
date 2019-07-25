fn test_clone() {
	a := [byte(0), 1, 2] 
	b := a.clone()
	assert b.len == 3
	assert b[0] == 0 
	assert b[1] == 1 
	assert b[2] == 2 
} 
