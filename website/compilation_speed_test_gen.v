// https://vlang.io/compilation_speed

// v gen.v && ./gen > out.c && time gcc out.c

const (
	N = 200
)

fn gen_c() {
	println('#include <stdio.h>\nint main() {')
	println('int a = 0;')
	for i := 0; i < N * 1000; i++ {
		println('a = $i; printf("%%d\\n", a);')
	}
	println('return 0; }')
}

fn gen_cpp() {
	println('#include <iostream>\nint main() {')
	println('int a = 0;')
	for i := 0; i < N * 1000; i++ {
		println('a = $i; std::cout << a << std::endl;')
	}
	println('return 0; }')
}

fn gen_swift() {
	println('var a = 0')
	for i := 0; i < N * 1000; i++ {
		println('a = $i; print(a)')
	}
}

fn gen_v() {
	println('fn main() { mut a := 0 ')
	for i := 0; i < N * 1000; i++ {
		println('a = $i')
	}
	println('}')
}

fn gen_rust() {
	println('fn main() { let mut a = 0; ')
	for i := 0; i < N * 1000; i++ {
		println('a = $i; println!("{}", a);')
	}
	println(' }')
}

fn gen_zig() {
	println('const warn = @import("std").debug.warn;
pub fn main() void { var a :i32 = 0;')
	for i := 0; i < N * 1000; i++ {
		println('a = $i; warn("{}", a);')
	}
	println('}')
}

fn gen_nim() {
	println(' var a  = 0')
	for i := 0; i < N * 1000; i++ {
		println('a = $i; echo(a);')
	}
}

fn main() {
	gen_c()
	//gen_cpp()
	//gen_swift()
	//gen_rust()
	//gen_nim()
	//gen_zig()
	//gen_v()
}

