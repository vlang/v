fn foo(a mut []int) {
	a[0] = 7 
	a << 4 
} 

// TODO 
fn test_mut() {
	mut a := [1,2,3] 
	foo(mut a) 
	//assert a.len == 4 
	assert a[0] == 7 
	//assert a[3] == 4 

	n := 1 
	mut b := &n 
	*b = 10 

	//mut b := mut a
	//b = 10 
} 
