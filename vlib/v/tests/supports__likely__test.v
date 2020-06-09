// _likely_(expr) should be compilable, and it should return the expr

fn test_likely_type(){
	assert typeof(_likely_(false)) == 'bool'
}    
    
fn test_likely(){
	if _likely_(2<10) {
		assert true
		eprintln('ok, happens every time')
	} else {
		eprintln('happens *infrequently*')
		assert false
	}
}    

fn test_likely_returns_the_value_of_its_bool_argument(){
	i := 123
	if _likely_(i<2) {
		assert false
	} else {
		assert true
	}
}
