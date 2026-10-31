module types

import os
import v.flat
import v.parser
import v.pref

fn test_implicit_integer_constant_guard_preserves_all_type_spellings() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.int_literal, '42')
	tc := TypeChecker.new(a)
	int_name := flat.source_type_int
	types := [
		Type(Void{}),
		Unknown{},
		Primitive{},
		Primitive{ props: .integer },
		Primitive{ props: .integer, size: 32 },
		Primitive{ props: .integer | .unsigned },
		Primitive{ props: .boolean | .integer },
		Primitive{ props: .float, size: 64 },
		String{},
		Char{},
		Rune{},
		ISize{},
		USize{},
		Nil{},
		None{},
		Array{ elem_type: &Type(int_) },
		ArrayFixed{ elem_type: &Type(int_), len: 2 },
		Channel{ elem_type: &Type(int_) },
		Map{ key_type: &Type(string_), value_type: &Type(int_) },
		Pointer{ base_type: &Type(int_) },
		FnType{ params: [FnParam{ typ: &Type(int_) }], return_type: &Type(int_) },
		OptionType{ base_type: &Type(int_) },
		ResultType{ base_type: &Type(int_) },
		MultiReturn{ types: [Type(int_), Type(string_)] },
		Struct{ name_id: int_name },
		Interface{ name_id: int_name },
		Enum{ name_id: int_name },
		SumType{ name_id: int_name },
		Alias{ name_id: int_name, base_type: &Type(usize_) },
		Alias{ name_id: flat.source_type_id('Count'), base_type: &Type(int_) },
		Struct{ name_id: flat.source_type_id('Value') },
	]
	for typ in types {
		value := tc.implicit_integer_constant_value(id, typ)
		if typ.name() == 'int' {
			assert value? == 42, typ.name()
		} else {
			assert value == none, typ.name()
		}
	}
}

fn test_call_argument_guards_preserve_diagnostics() {
	cases := [
		'fn accept(value u8) {}\nfn main() { accept(255); accept(256); accept(-1) }',
		'type Count = int\nfn accept(value u8) {}\nfn main() { count := Count(2); accept(count) }',
		'fn accept(value uint) {}\nfn main() { size := sizeof(int); accept(size) }',
		'fn accept(value int) {}\nfn main() { value := 4; accept(value) }',
		'fn accept(value []int) {}\nfn main() { value := [4]; accept(value) }',
		'fn accept(value fn (int) int) {}\nfn identity(value int) int { return value }\nfn main() { accept(identity) }',
		'fn accept(value &int) {}\nfn main() { value := unsafe { nil }; accept(value); accept(unsafe { nil }) }',
		'interface Reader { read() int }\nfn accept(value Reader) {}\nfn main() { value := unsafe { nil }; accept(value); accept(unsafe { nil }) }',
		'fn accept(value int) {}\nfn main() { value := unsafe { nil }; accept(value) }',
		'struct Cell { mut: value int }\nfn accept(value voidptr) {}\nfn main() { value := Cell{}; accept(value) }',
		'struct Cell { mut: value int }\nfn accept(value voidptr) {}\nfn caller(mut value Cell) { accept(value) }\nfn main() { mut value := Cell{}; caller(mut value) }',
		'struct Cell { mut: value int }\nfn accept(value voidptr) {}\nfn (mut value Cell) call() { accept(value) }\nfn main() { mut value := Cell{}; value.call() }',
	]
	for index, source in cases {
		for scoped in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'call_argument_guard_${index}_${os.getpid()}.v')
			os.write_file(path, 'module main\n' + source + '\n')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, '${index}: ${p.diagnostics}'
			mut tc := TypeChecker.new(a)
			tc.scope_parallel_check_workers = scoped
			tc.collect(a)
			_ = tc.check_semantics_opt(false)
			os.rm(path) or {}
		}
	}
}
