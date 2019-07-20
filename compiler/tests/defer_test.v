fn foo() string {
	println('foo()') 
	return 'foo' 
} 

fn foo2() string { 
	println('start') 
	defer { println('defer') } 
	println('end') 
	return foo() 
} 

fn test_defer() { 
	assert foo2() == 'foo' 
} 
