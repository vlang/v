import os
import wasm

// Install Wasmer:
// $ curl https://get.wasmer.io -sSfL | sh
// $ wasmer --version
//
// Create `examples/wasm_codegen/math.wasm`:
// $ v run examples/wasm_codegen/main.v
//
// Call entrypoints with the current Wasmer CLI:
// $ wasmer run examples/wasm_codegen/math.wasm --invoke add 3 5
// 8
// $ wasmer run examples/wasm_codegen/math.wasm --invoke factorial 10
// 3628800
// $ wasmer run examples/wasm_codegen/math.wasm --invoke pythagoras 30 40
// 50
fn main() {
	mut math := Math{}
	math.function_add('add')
	math.function_factorial('factorial')
	math.function_pythagoras('pythagoras')
	math.save(os.join_path(@DIR, 'math.wasm'))!
}

struct Math {
	wasm.Module
}

fn (mut math Math) function_add(name string) {
	mut func := math.new_function(name, [.i32_t, .i32_t], [.i32_t])
	func.local_get(0)
	func.local_get(1)
	func.add(.i32_t)
	math.commit(func, true)
}

fn (mut math Math) function_factorial(name string) {
	mut func := math.new_function(name, [.i64_t], [.i64_t])
	func.local_get(0)
	func.eqz(.i64_t)
	block := func.c_if([], [.i64_t])
	func.i64_const(1)
	func.c_else(block)
	func.local_get(0)
	func.local_get(0)
	func.i64_const(1)
	func.sub(.i64_t)
	func.call(name)
	func.mul(.i64_t)
	func.c_end(block)
	math.commit(func, true)
}

fn (mut math Math) function_pythagoras(name string) {
	mut func := math.new_function(name, [.f32_t, .f32_t], [.f64_t])
	func.local_get(0)
	func.local_get(0)
	func.mul(.f32_t)
	func.local_get(1)
	func.local_get(1)
	func.mul(.f32_t)
	func.add(.f32_t)
	func.sqrt(.f32_t)
	func.cast(.f32_t, true, .f64_t)
	math.commit(func, true)
}

fn (mut math Math) save(path string) ! {
	os.write_file_array(path, math.compile())!
}
