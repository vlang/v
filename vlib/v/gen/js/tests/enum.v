import hello

enum Test {
	foo = 2
	bar = 5
	baz
}

mut a := hello.Ccc.a
a = .b
a = .c
println(a)


mut b := Test.foo
b = .bar
println(b)
