import os
import v.cmdexec

const scope_review_program = 'module main\n\nstruct T {\n\tvalue int\n}\n\nfn identity[T](x T) T {\n\treturn x\n}\n\n__global number T = T{}\n\nstruct Holder[T] {\n\titem T\n}\n\n__global after_struct T = T{}\n\ntype Values[T] = []T\n\n__global after_alias T = T{}\n\nfn consume(x T) T {\n\treturn x\n}\n\nfn main() {\n\tprintln(number.value)\n}\n'

fn scope_review_query(path string, position string, method string) string {
	query := '${path}:${position.all_before(':')}:${method}^${position.all_after(':')}'
	result := cmdexec.run(@VEXE, ['-new-compiler', '-no-memory-limit', '-enable-globals', '-check',
		'-vls-mode', '-line-info', query, path])
	assert result.exit_code == 0, result.output
	return result.output.trim_space()
}

fn test_type_parameter_hover_and_definition_stay_in_their_declaration_scope() {
	root := os.join_path(os.vtmp_dir(), 'vls_generic_scope_review_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'scope.v')
	os.write_file(path, scope_review_program)!
	for position in ['11:16', '17:22', '21:21', '23:13'] {
		hover := scope_review_query(path, position, 'hv')
		assert hover.contains('struct T'), hover
		assert !hover.contains('[T]'), hover
		assert scope_review_query(path, position, 'gd') == '${path}:3:7'
	}
	for position in ['7:12', '7:17', '7:20'] {
		assert scope_review_query(path, position, 'hv').contains('[T]')
		assert scope_review_query(path, position, 'gd') == '${path}:7:12'
	}
	for position in ['13:14', '14:6'] {
		assert scope_review_query(path, position, 'hv').contains('[T]')
		assert scope_review_query(path, position, 'gd') == '${path}:13:14'
	}
	assert scope_review_query(path, '19:19', 'hv').contains('[T]')
	assert scope_review_query(path, '19:19', 'gd') == '${path}:19:12'
}

// receiver_scope_layouts put right before a method of `Box[T]` each kind of
// declaration that the `T` written in its receiver was taken from: its struct,
// another method, and a generic function with and without a `T` of its own.
const receiver_scope_layouts = [
	'',
	'fn (b Box[T]) first() T {\n\treturn b.value\n}\n\n',
	'fn identity[T](x T) T {\n\treturn x\n}\n\n',
	'fn helper[U](u U) U {\n\treturn u\n}\n\n',
]

const receiver_forms_program = 'module main\n\nstruct T {\n\tvalue int\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nstruct Pair[K, V] {\n\tkey   K\n\tvalue V\n}\n\nfn (p Pair[K, V]) entry() (K, V) {\n\treturn p.key, p.value\n}\n\nfn (mut b Box[T]) set(value T) {\n\tb.value = value\n}\n\nfn (b &Box[T]) peek() T {\n\treturn b.value\n}\n\nfn (b Box[T]) convert[U](f fn (T) U) Box[U] {\n\treturn Box[U]{\n\t\tvalue: f(b.value)\n\t}\n}\n\n__global after_methods T = T{}\n\nfn main() {\n\tprintln(after_methods.value)\n}\n'

// word_position is `line:column` of the `nth` type parameter `name` in the
// first line of `source` that starts with `prefix`.
fn word_position(source string, prefix string, name string, nth int) string {
	mut seen := 0
	for i, line in source.split_into_lines() {
		if !line.starts_with(prefix) {
			continue
		}
		for col, c in line {
			if c == name[0] && (col == 0 || !line[col - 1].is_alnum())
				&& (col + 1 == line.len || !line[col + 1].is_alnum()) {
				if seen == nth {
					return '${i + 1}:${col}'
				}
				seen++
			}
		}
		break
	}
	panic('no `${name}` #${nth} in the line `${prefix}`')
}

fn test_a_type_parameter_in_a_method_receiver_belongs_to_the_method() {
	root := os.join_path(os.vtmp_dir(), 'vls_receiver_scope_review_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for i, layout in receiver_scope_layouts {
		path := os.join_path(root, 'layout_${i}.v')
		source := 'module main\n\nstruct Box[T] {\n\tvalue T\n}\n\n${layout}fn (b Box[T]) get() T {\n\t_ := []T{}\n\treturn b.value\n}\n\nfn main() {\n\tprintln(Box[int]{}.get())\n}\n'
		os.write_file(path, source)!
		receiver := word_position(source, 'fn (b Box[T]) get', 'T', 0)
		for position in [receiver, word_position(source, 'fn (b Box[T]) get', 'T', 1),
			word_position(source, '\t_ := []T{}', 'T', 0)] {
			assert scope_review_query(path, position, 'hv').contains('[T]'), 'layout ${i} at ${position}'
			assert scope_review_query(path, position, 'gd') == '${path}:${receiver}', 'layout ${i} at ${position}'
		}
		if layout.starts_with('fn (b Box[T]) first') {
			first := word_position(source, 'fn (b Box[T]) first', 'T', 0)
			assert scope_review_query(path, first, 'gd') == '${path}:${first}'
		}
	}
}

fn test_type_parameters_in_receivers_of_every_form_belong_to_their_method() {
	root := os.join_path(os.vtmp_dir(), 'vls_receiver_forms_review_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'forms.v')
	source := receiver_forms_program
	os.write_file(path, source)!
	for prefix, names in {
		'fn (p Pair[K, V]) entry': ['K', 'V']
		'fn (mut b Box[T]) set':   ['T']
		'fn (b &Box[T]) peek':     ['T']
		'fn (b Box[T]) convert':   ['T']
	} {
		for name in names {
			declared := word_position(source, prefix, name, 0)
			hover := scope_review_query(path, declared, 'hv')
			assert hover.contains('[${name}]'), '${prefix}: ${hover}'
			assert !hover.contains('struct'), '${prefix}: ${hover}'
			assert scope_review_query(path, declared, 'gd') == '${path}:${declared}', prefix
			// Its next use in the signature leads to the receiver too.
			used := word_position(source, prefix, name, 1)
			assert scope_review_query(path, used, 'gd') == '${path}:${declared}', '${prefix} ${used}'
		}
	}
	// The own type parameter of the method is declared after its name.
	own := word_position(source, 'fn (b Box[T]) convert', 'U', 0)
	result := word_position(source, 'fn (b Box[T]) convert', 'U', 2)
	assert scope_review_query(path, result, 'gd') == '${path}:${own}'
	// A declaration after the methods still finds the module type `T`.
	after := word_position(source, '__global after_methods', 'T', 0)
	hover := scope_review_query(path, after, 'hv')
	assert hover.contains('struct T') && !hover.contains('[T]'), hover
	assert scope_review_query(path, after, 'gd') == '${path}:3:7'
}
