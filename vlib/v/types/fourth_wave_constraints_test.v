module types

import os
import v.flat

fn fourth_constraint_program(name string, source string, flags string) os.Result {
	return fourth_constraint_program_with_module(name, source, flags, 'module limits\npub type Number = int | i64\n')
}

fn fourth_constraint_program_with_module(name string, source string, flags string, dependency string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'fourth_constraint_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'limits')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fourth_constraints' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'limits', 'limits.v'), dependency) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	return os.exec([@VEXE, '-new-compiler', '-check', '-nocolor',
		...(os.split_args(flags) or { panic(err) }), root])
}

fn test_fourth_constraints_count_imported_constraint_types_as_used() {
	for name, prefix in {
		'qualified':   'import limits\nfn accept[T limits.Number](value T) T { return value }'
		'aliased':     'import limits as bound\nfn accept[T bound.Number](value T) T { return value }'
		'selective':   'import limits { Number }\nfn accept[T Number](value T) T { return value }'
		'local_alias': 'import limits\ntype Number = limits.Number\nfn accept[T Number](value T) T { return value }'
	} {
		for flags in ['-W', '-W -no-parallel'] {
			result := fourth_constraint_program(name, 'module main\n${prefix}\nfn main() { println(accept(1)) }\n', flags)
			assert result.exit_code == 0, result.output
		}
	}
	invalid := fourth_constraint_program('invalid_bound', 'module main
import limits
fn accept[T limits.Number](value T) T { return value }
fn main() { println(accept("bad")) }
', '-W')
	assert invalid.exit_code != 0, invalid.output
	assert invalid.output.contains('it is not in its constraint `limits.Number`'), invalid.output
	assert !invalid.output.contains('imported but never used'), invalid.output
	unused := fourth_constraint_program('unused', 'module main
import limits
fn accept[T int](value T) T { return value }
fn main() { println(accept(1)) }
', '-W')
	assert unused.exit_code != 0, unused.output
	assert unused.output.contains('imported but never used'), unused.output
}

fn test_fourth_constraints_reject_growing_sum_declarations() {
	for name, declaration in {
		'array_growth':          'type Part[T] = T | bool\ntype Expanding[T] = T | Part[Expanding[[]T]]'
		'mutual_growth':         'type Part[T] = T | bool\ntype Expanding[T] = T | Part[Other[[]T]]\ntype Other[T] = T | Part[Expanding[[]T]]'
		'alias_growth':          'type Part[T] = T | bool\ntype Growing[T] = []T\ntype Expanding[T] = T | Part[Expanding[Growing[T]]]'
		'identity_alias_growth': 'type Part[T] = T | bool\ntype Identity[T] = T\ntype Expanding[T] = T | Part[Expanding[Identity[T]]]'
		'constant_alias_growth': 'type Part[T] = T | bool\ntype Drop[T] = int\ntype Expanding[T] = T | Part[Expanding[Drop[T]]]'
	} {
		result := fourth_constraint_program(name, 'module main
${declaration}
fn accept[T Expanding[int]](value T) T { return value }
fn main() { println(accept([]int{})) }
', '')
		assert result.exit_code != 0, result.output
		assert result.output.contains('constraint `Expanding[int]` cannot expand a recursive sum type'), result.output
		assert !result.output.contains('it is not in its constraint'), result.output
	}
}

fn test_fourth_constraints_keep_finite_nested_aliases_and_regular_cycles() {
	finite := fourth_constraint_program('finite', 'module main
type Part[T] = T | bool
type First = Part[int]
type Values = Part[First] | Part[Part[string]]
fn accept[T Values](value T) T { return value }
fn main() { println(accept(1)); println(accept("ok")); println(accept(true)) }
', '')
	assert finite.exit_code == 0, finite.output
	regular := fourth_constraint_program('regular', 'module main
type Part[T] = T | bool
type Recursive[T] = T | Part[Recursive[T]]
fn accept[T Recursive[int]](value T) T { return value }
fn main() { println(accept(1)); println(accept(true)) }
', '')
	assert regular.exit_code == 0, regular.output
}

fn test_fourth_constraints_accept_deep_struct_embedding_families() {
	mut source := 'module main\nstruct Base { n int }\n'
	mut previous := 'Base'
	for i in 1 .. 51 {
		name := 'Wrap${i}'
		source += 'struct ${name} { ${previous} }\n'
		previous = name
	}
	source += 'fn accept[T Base](value T) int { return value.n }\nfn main() { println(accept(Wrap50{})) }\n'
	result := fourth_constraint_program('deep', source, '')
	assert result.exit_code == 0, result.output
}

fn test_fourth_constraints_struct_embedding_cycles_terminate() {
	tc := TypeChecker{
		a:                      &flat.FlatAst{}
		struct_embed_receivers: {
			'CycleA': ['CycleB', 'Exit']
			'CycleB': ['CycleA']
			'Exit':   ['Base']
		}
	}
	assert tc.struct_embeds('CycleA', 'Base')
	assert !tc.struct_embeds('CycleA', 'Missing')
}

fn test_fourth_constraints_preserve_finite_changing_recursive_instances() {
	for name, definition in {
		'constant':                   'type Recursive[T] = T | Part[Recursive[int]]\nfn accept[T Recursive[string]](value T) T { return value }\nfn main() { println(accept("ok")); println(accept(1)); println(accept(true)) }'
		'constant_array':             'type Recursive[T] = T | Part[Recursive[[]int]]\nfn accept[T Recursive[int]](value T) T { return value }\nfn main() { println(accept(1)); println(accept([]int{})); println(accept(true)) }'
		'permutation':                'type Recursive[T, U] = T | Part[Recursive[U, T]]\nfn accept[T Recursive[int, string]](value T) T { return value }\nfn main() { println(accept(1)); println(accept("ok")); println(accept(true)) }'
		'acyclic_replacement':        'type Recursive[T, U] = T | Part[Recursive[[]U, int]]\nfn accept[T Recursive[int, string]](value T) T { return value }\nfn main() { println(accept(1)); println(accept([]string{})); println(accept([]int{})) }'
		'constant_alias_replacement': 'type Identity[T] = T\ntype Recursive[T] = T | Part[Recursive[Identity[int]]]\nfn accept[T Recursive[string]](value T) T { return value }\nfn main() { println(accept("ok")); println(accept(Identity[int](1))); println(accept(true)) }'
	} {
		result := fourth_constraint_program(name, 'module main\ntype Part[T] = T | bool\n${definition}\n', '')
		assert result.exit_code == 0, result.output
	}
}

fn test_fourth_constraints_ignore_growth_in_atomic_or_unused_arguments() {
	for name, declaration in {
		'array':           'type Values[T] = int | []Values[[]T]'
		'unused_argument': 'type Dormant[U] = bool | string\ntype Values[T] = T | Dormant[Values[[]T]]'
	} {
		result := fourth_constraint_program(name, 'module main\n${declaration}\nfn accept[T Values[int]](value T) T { return value }\nfn main() { println(accept(1)) }\n', '')
		assert result.exit_code == 0, result.output
	}
}

fn test_fourth_constraints_parameter_dependencies_require_a_growing_cycle() {
	assert sum_constraint_arguments_expand(['[]T'], ['T'])
	assert sum_constraint_arguments_expand(['U', '[]T'], ['T', 'U'])
	assert sum_constraint_arguments_expand(['bool', '[][]U'], ['T', 'U'])
	assert !sum_constraint_arguments_expand(['[]int'], ['T'])
	assert !sum_constraint_arguments_expand(['U', 'T'], ['T', 'U'])
	assert !sum_constraint_arguments_expand(['[]U', 'int'], ['T', 'U'])
	assert !sum_constraint_arguments_expand(['[]U', '[]V', 'bool'], ['T', 'U', 'V'])
}

fn test_fourth_constraints_reject_composed_parameter_growth() {
	result := fourth_constraint_program('composed_growth', 'module main
type Part[T] = T | bool
type Expanding[T, U] = T | Part[Expanding[[]U, int]] | Part[Expanding[int, []T]]
fn accept[T Expanding[int, int]](value T) T { return value }
fn main() { println(accept(1)) }
', '')
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot expand a recursive sum type'), result.output
}

fn test_fourth_constraints_preserve_distinct_alias_members() {
	positive := fourth_constraint_program('alias_member', 'module main
type Integer = int
type Part[T] = T | bool
fn accept[T Part[Integer]](value T) T { return value }
fn main() { println(accept(Integer(1))); println(accept(true)) }
', '')
	assert positive.exit_code == 0, positive.output
	negative := fourth_constraint_program('alias_member_invalid', 'module main
type Integer = int
type Part[T] = T | bool
fn accept[T Part[Integer]](value T) T { return value }
fn main() { println(accept(1)) }
', '')
	assert negative.exit_code != 0, negative.output
	assert negative.output.contains('it is not in its constraint `Part[Integer]`'), negative.output
	separate := fourth_constraint_program('alias_and_plain_member', 'module main
type Integer = int
type Part[T] = T | bool
type Values = Part[Integer] | Part[int]
fn accept[T Values](value T) T { return value }
fn main() { println(accept(Integer(1))); println(accept(1)); println(accept(true)) }
', '')
	assert separate.exit_code == 0, separate.output
}

fn test_fourth_constraints_keep_symbolic_cycles_in_imported_modules() {
	for name, definition in {
		'permutation': 'pub type Recursive[T, U] = T | Part[Recursive[U, T]]\npub fn accept[T Recursive[int, string]](value T) T { return value }'
		'constant':    'pub type Recursive[T] = T | Part[Recursive[int]]\npub fn accept[T Recursive[string]](value T) T { return value }'
	} {
		result := fourth_constraint_program_with_module('imported_${name}', 'module main\nimport limits\nfn main() { println(limits.accept(1)); println(limits.accept("ok")) }\n', '', 'module limits\npub type Part[T] = T | bool\n${definition}\n')
		assert result.exit_code == 0, result.output
	}
	growing := fourth_constraint_program_with_module('imported_growth', 'module main\nimport limits\nfn main() { println(limits.accept(1)) }\n', '', 'module limits
pub type Part[T] = T | bool
pub type Expanding[T] = T | Part[Expanding[[]T]]
pub fn accept[T Expanding[int]](value T) T { return value }
')
	assert growing.exit_code != 0, growing.output
	assert growing.output.contains('constraint `Expanding[int]` cannot expand a recursive sum type'), growing.output
}
