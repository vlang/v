module parser

import os
import v.pref

fn test_formatter_named_struct_fields_span_their_key_tokens() {
	path := os.join_path(os.vtmp_dir(), 'formatter_struct_field_spans_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	source := 'struct Host { hello int }\nfn main() { h := Host{hello: 42}; _ := Host{...h, hello: 43} }\n'
	os.write_file(path, source)!
	for is_fmt in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.is_fmt = is_fmt
		mut p := Parser.new(prefs)
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		fields := a.nodes.filter(it.kind == .field_init && it.value == 'hello')
		assert fields.len == 2
		for field in fields {
			assert source[field.pos.offset..field.pos.end] == if is_fmt { 'hello' } else { '}' }
		}
	}
}

fn test_formatter_preserves_syntax_without_semantic_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'formatter_semantics_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	cases := {
		'fn f(ch chan) {}':                                                                          '`chan` has no type specified. Use `chan Type` instead of `chan`'
		'fn main() { ch := chan{}; _ = ch }':                                                        '`chan` has no type specified. Use `chan Type{}` instead of `chan{}`'
		'fn f() { match sql { else {} } }':                                                          'unexpected keyword `sql`, expecting name'
		'fn f(sql int) {}':                                                                          'unexpected keyword `sql`, expecting name'
		'fn f(value, sql int) {}':                                                                   'unexpected keyword `sql`, expecting name'
		'fn f[T](value T) [T] { return value }':                                                     'invalid generic return, use `T` instead'
		'fn main() { asm arm64 intel { nop } }':                                                     'the `intel` assembly modifier is only supported for i386 and amd64'
		'fn f(value int) { if match value { 0 { true } else { false } } {} }':                       'cannot use `match` with `if` statements'
		'fn f(values []?int) ?int { return values[0]? }':                                            '`?` for propagating errors from index expressions is no longer supported, use `!` instead of `?`'
		'@[inline] @[deprecated] fn f() {}':                                                         'multiple attributes should be in the same @[], with ; separators'
		"struct Holder { value int @[required] @[json: 'value'] }":                                  'multiple attributes should be in the same @[], with ; separators'
		'fn f[T]() { if T is int { println(1) } }':                                                  'use `$if` instead of `if`'
		'fn main() { for i := 0; i < 3; j := 1 { println(j) } }':                                    'for loop post statement cannot be a variable declaration'
		'fn main() {}\n#!/usr/bin/env -S v run':                                                     'a shebang is only valid at the top of the file'
		'fn f(x int) { match x { 0 .. 3 {} else {} } }':                                             'match only supports inclusive (`...`) ranges, not exclusive (`..`) '
		'struct Holder { pkg.lower }':                                                               'invalid field name'
		'struct Example {}\nfn (value Example) Foo.bar() {}':                                        'cannot declare a static function as a receiver method'
		'fn main() { for i in 0 ... 3 { println(i) } }':                                             'for loop only supports exclusive (`..`) ranges, not inclusive (`...`)'
		'fn f(value array) {}':                                                                      '`array` is an internal type, it cannot be used directly. Use `[]int`, `[]Foo` etc'
		'fn f(value map) {}':                                                                        'cannot use the map type without key and value definition'
		'fn main() { _ := [2]map{} }':                                                               'cannot use the map type without key and value definition'
		'fn main() { asm amd64 raw raw { nop } }':                                                   'duplicate `raw` assembly modifier'
		'fn main() { asm amd64 intel intel { nop } }':                                               'duplicate `intel` assembly modifier'
		'fn main() { asm amd64 { lock nop } }':                                                      'The lock prefix cannot be used on this instruction'
		'struct Holder { value mut int }':                                                           'cannot use `mut` on struct field type'
		'fn main() { callback := fn [missing] () {}; _ = callback }':                                'undefined ident: `missing`'
		'interface Reader { read[T](value T) T }':                                                   'non-generic interface `Reader` cannot define a generic method'
		'fn loops(values []int) { for mut index, _ in values { index++ } }':                         'index of array or key of map cannot be mutated'
		'fn loops() { for mut index in 0 .. 3 { index++ } }':                                        'variable in range `for` cannot be mut'
		'fn loops() { for index, value in 0 .. 3 { _ = value } }':                                   'cannot declare index variable with range `for`'
		'interface Abc { fun(); fun() }':                                                            'duplicate method `fun`'
		'fn loops(values []int) { val := 1; for val in values { _ = val } }':                        'redefinition of value iteration variable `val`, use `for (val in array) {` if you want to check for a condition instead'
		'fn closure() { x := 1; callback := fn [x] (x int) {} }':                                    'the parameter name `x` conflicts with the captured value name'
		'interface Reader { read[T, T](value T) T }':                                                'duplicated generic parameter `T`'
		'fn many[A, B, C, D, E, F, G, H, I, J]() {}':                                                'cannot have more than 9 generic parameters'
		'fn main() { if x := 1 { _ = x } }':                                                         'if guard condition expression is illegal, it should return an Option'
		'fn maybe_value() ?int { return 1 }\nfn main() { x := 1; if x := maybe_value() { _ = x } }': 'redefinition of `x`'
		'struct B {}\nstruct A { B; B }':                                                            'cannot embed `B` more than once'
		'struct Number {}\nfn (n Number) += (other Number) Number { return n }':                     'cannot overload `+=`, overload `+` and `+=` will be automatically generated'
		'fn handle(int) {}':                                                                         'functions with type only params can not have bodies'
		'fn main() { a := []int{init: 1}; _ = a }':                                                  'cannot use `init` attribute unless `len` attribute is also provided'
		'fn main() { select { else {} else {} } }':                                                  'at most one `else` branch allowed in `select` block'
		'fn main() { unsafe { unsafe { println(1) } } }':                                            'already inside `unsafe` block'
		"@[deprecated(msg: 'old', msg: 'new')] fn old() {}":                                         'duplicate `msg` argument for `@[deprecated(...)]` attribute'
		'fn + (a int, b int) int { return a + b }':                                                  'cannot use operator overloading with normal functions'
		"@[export: 'f'] fn f[T](value T) {}":                                                        'generic functions cannot be exported'
		"@[export: 'f'] fn C.f()":                                                                   'interop function cannot be exported'
		'fn C.foo() {}':                                                                             'interop functions cannot have a body'
		'fn f(mut values ...int) {}':                                                                'variadic arguments cannot be `mut`, `shared` or `atomic`'
		'fn f(shared values ...int) {}':                                                             'variadic arguments cannot be `mut`, `shared` or `atomic`'
		'fn f(atomic values ...int) {}':                                                             'variadic arguments cannot be `mut`, `shared` or `atomic`'
		'type Callback = fn (mut ...int)':                                                           'variadic arguments cannot be `mut`, `shared` or `atomic`'
		'fn main() { values := []!int{}; _ = values }':                                              'arrays do not support storing Result values'
		'fn main() { values := [2]!int{}; _ = values }':                                             'fixed arrays do not support storing Result values'
		'fn main() { values := [..]!int[1, 2]; _ = values }':                                        'fixed arrays do not support storing Result values'
		'fn main() { values := chan !int{}; _ = values }':                                           'cannot use chan with Result type'
		'fn main() { values := [2]int{len: 2}; _ = values }':                                        '`len` and `cap` are invalid attributes for fixed array dimension'
		'fn f() mut int { return 1 }':                                                               'cannot use `mut` on fn return type'
		'__global int int':                                                                          'invalid use of reserved type `int` as a global name'
		'fn main() { mut _ := 1 }':                                                                  'cannot use `mut` on `_`'
		'fn main() { shared _ := 1 }':                                                               'cannot use `shared` on `_`'
		'fn main() { atomic _ := 1 }':                                                               'cannot use `atomic` on `_`'
		'fn main() { mut x := 1; x = 2 @[freed; other] }':                                           'assignment attributes support at most one argument'
		'fn main() { mut x := 1; x = 2 @[freed: 1] }':                                               'assignment attribute `freed` does not accept an argument'
		'fn main() { println($res()) }':                                                             '`println` can not print void expressions'
		'fn f(xs ...int, y int) {}':                                                                 'cannot use ...(variadic) with non-final parameter xs'
		'fn f[T]() { $for field in T.unknown {} }':                                                  'unknown kind `unknown`, available are: `methods`, `fields`, `values`, `variants`, `attributes` or `params`'
	}
	for source, expected in cases {
		os.write_file(path, source + '\n')!
		mut compiler := Parser.new(pref.new_preferences())
		compiler.parse_file(path)
		assert compiler.diagnostics.any(it.message == expected), compiler.diagnostics.str()

		mut prefs := pref.new_preferences()
		prefs.is_fmt = true
		mut formatter := Parser.new(prefs)
		formatter.parse_file(path)
		assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
	}
}

fn test_formatter_preserves_interop_declarations_in_other_backend_files() {
	for suffix, prefix in {
		'c':  'JS'
		'js': 'C'
	} {
		path := os.join_path(os.vtmp_dir(), 'formatter_interop_${os.getpid()}.${suffix}.v')
		defer { os.rm(path) or {} }
		os.write_file(path, 'fn ${prefix}.alert()\n')!
		mut compiler := Parser.new(pref.new_preferences())
		compiler.parse_file(path)
		assert compiler.diagnostics.any(it.message.contains('code is not allowed')), compiler.diagnostics.str()
		mut prefs := pref.new_preferences()
		prefs.is_fmt = true
		mut formatter := Parser.new(prefs)
		formatter.parse_file(path)
		assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
	}
}

fn test_formatter_still_reports_invalid_syntax() {
	path := os.join_path(os.vtmp_dir(), 'formatter_invalid_syntax_${os.getpid()}.v')
	os.write_file(path, 'fn main() { value := }\n')!
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut formatter := Parser.new(prefs)
	formatter.parse_file(path)
	assert formatter.diagnostics.len > 0
}

fn test_formatter_preserves_deep_assignment_expressions() {
	path := os.join_path(os.vtmp_dir(), 'formatter_deep_assignment_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	mut expression := '1'
	for _ in 0 .. 101 {
		expression = '1 + (${expression})'
	}
	os.write_file(path, 'fn main() { value := ${expression}; _ = value }\n')!
	mut compiler := Parser.new(pref.new_preferences())
	compiler.parse_file(path)
	assert compiler.diagnostics.any(it.message == 'expr level > 100'), compiler.diagnostics.str()
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut formatter := Parser.new(prefs)
	formatter.parse_file(path)
	assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
}

fn test_formatter_skips_assembly_backend_compatibility() {
	path := os.join_path(os.vtmp_dir(), 'formatter_asm_backend_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for backend in ['wasm', 'arm64', 'eval'] {
		for source, expected in {
			'fn main() { asm goto arm64 { nop } }':  '`asm goto` is only supported by the C backend'
			'fn main() { asm amd64 raw { "nop" } }': 'the `raw` assembly modifier is only supported by the C backend'
			'fn main() { asm amd64 intel { nop } }': 'the `intel` assembly modifier is only supported by the C backend'
		} {
			os.write_file(path, source + '\n')!
			mut prefs := pref.new_preferences()
			prefs.backend = backend
			mut compiler := Parser.new(prefs)
			compiler.parse_file(path)
			assert compiler.diagnostics.any(it.message == expected), compiler.diagnostics.str()
			prefs.is_fmt = true
			mut formatter := Parser.new(prefs)
			formatter.parse_file(path)
			assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
		}
	}
}
