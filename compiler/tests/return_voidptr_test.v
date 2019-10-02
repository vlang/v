
struct Zest { val int }
    
fn (t Zest) get_a_finger_to_the_moon() voidptr {
	return voidptr(0)
}
        
fn test_returning_a_void_pointer_from_a_method() {
	t := &Zest{ val: 123 }
	
	mut z := voidptr(0)
	z = t.get_a_finger_to_the_moon()
	C.printf( "%x\n", z)

	assert t.get_a_finger_to_the_moon() == 0
}
