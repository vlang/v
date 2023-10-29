// declare interface
interface MyInterface {
	val() int
}

// define struct type
struct St {
mut:
	n int
}

// make the struct type implement the interface
fn (x St) val() int {
	return x.n
}

fn overwrite_stack() f64 {
	a := 12.5
	b := 3.5
	c := a + b
	return c
}

// nothing special so far, but now some functions that return an interfaces
// these used to cause memory corruptions, but work with this PR:
fn gen_interface() MyInterface {
	x := St{
		n: -123
	} // `x`will be allocated on heap
	return x // because an interface object is returned here that contains the address of x
}

fn return_interface(x St) MyInterface {
	// x will be copied to stack (requires #10528)
	return x // because it's address is returned inside the interface
}

fn test_gen_interface() {
	i1 := gen_interface()
	d := overwrite_stack()
	assert i1.val() == -123
	assert d == 16.0
}

fn test_convert_to_interface() {
	x := St{
		n: 5
	}
	i2 := return_interface(x)
	d := overwrite_stack()
	assert i2.val() == 5
	assert d == 16.0
}
