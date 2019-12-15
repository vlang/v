module amodule

// this tests whether _test.v files can be *internal* 
// to a module, and thus have access to its guts.

fn test_iadd(){
	assert iadd(10, 20) == 30
}

fn test_imul(){
	assert imul(5,8) == 40
}

fn test_private_isub(){
  assert private_isub(10,6) == 4
}
