module main

fn test_interface_embedding_call() {
	g1 := G1{}
	do_the_greet(g1)
}

struct G1 {}

fn (g G1) greet() string {
	return 'hello from G1'
}

fn do_the_greet(g ParentGreeter) {
	greet := g.greet()
	println('Someone says: $greet')
	assert greet == 'hello from G1'
}

interface ParentGreeter {
	Greeter
}

interface Greeter {
	greet() string
}
