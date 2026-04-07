// Tests that methods of generic structs implementing generic interfaces
// are properly type-checked when the struct is instantiated indirectly
// (e.g. through a function that constructs the struct and returns it as
// the interface type). Previously, generic_insts_to_concrete() would
// register concrete types for these methods only after the checker's
// post_process_generic_fns loop had already finished, causing cgen to
// attempt code generation for unchecked AST nodes.

interface Processor[T] {
	process(val T) T
}

struct Doubler[T] {
	factor T
}

fn (d &Doubler[T]) process(val T) T {
	return val * d.factor
}

struct Negator[T] {
	offset T
}

fn (n &Negator[T]) process(val T) T {
	return -val + n.offset
}

fn make_doubler[T](factor T) Processor[T] {
	return Processor[T](&Doubler[T]{
		factor: factor
	})
}

fn make_negator[T](offset T) Processor[T] {
	return Processor[T](&Negator[T]{
		offset: offset
	})
}

struct Pipeline[T] {
mut:
	processors []Processor[T]
}

fn (mut p Pipeline[T]) add_doubler(factor T) {
	p.processors << make_doubler[T](factor)
}

fn (mut p Pipeline[T]) add_negator(offset T) {
	p.processors << make_negator[T](offset)
}

fn (p &Pipeline[T]) run(val T) T {
	mut result := val
	for proc in p.processors {
		result = proc.process(result)
	}
	return result
}

fn test_generic_interface_cross_module_recheck() {
	mut p := Pipeline[f64]{}
	// Only add_doubler is called; add_negator is never called.
	// But Negator[f64] still gets instantiated via generic_insts_to_concrete
	// and its process method must be properly type-checked.
	p.add_doubler(3.0)
	assert p.run(5.0) == 15.0
}
