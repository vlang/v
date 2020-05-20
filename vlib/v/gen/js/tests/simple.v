import hello
// import hello.hello1
// TODO: Uncomment once nested modules work

fn JS.console.log()

struct D {
    a hello.A
}

fn struct_arg (arg hello.A) {
	JS.console.log(arg)
}

fn main() {

	struct_arg(hello.A{ 'hello' })

	mut a := 1
	a += 2
	JS.console.log(a)
	b := hello.A{}
	JS.console.log(b)
	// hello1.nested()
}
