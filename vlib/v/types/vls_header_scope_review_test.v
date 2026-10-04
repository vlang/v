module types

import v.flat
import v.token

fn test_bodyless_generic_header_does_not_capture_a_following_global_type() {
	for signature in ['identity[T](x T) T', 'identity[T](\n\tx T\n) T'] {
		for separator in ['\n', '; ', ' '] {
			source := '${signature}${separator}__global number T = T{}\n'
			mut a := flat.FlatAst.new()
			mut file_set := token.FileSet.new()
			a.source_files[1] = file_set.add_file('scope.vh', source.len)
			mut tc := TypeChecker.new(&a)
			tc.source_texts_by_file['scope.vh'] = source
			decl := flat.Node{
				kind:   .fn_decl
				pos:    token.new_pos(1, 0)
				is_mut: true
			}
			inside := signature.last_index('T') or { panic('missing signature T') }
			outside := source.index('number T') or { panic('missing global T') }
			assert tc.vls_decl_contains_offset(decl, inside)
			assert !tc.vls_decl_contains_offset(decl, outside + 7)
		}
	}
}

fn test_bodyless_generic_header_keeps_function_return_types_in_scope() {
	for signature, return_type in {
		'maker[T](x T) fn (T) T':               'fn(T) T'
		'maker[T](x fn (T) T) fn (T) fn (T) T': 'fn(T) fn(T) T'
		'maker[T](x T) (fn (T) T, fn (T) T)':   '(fn(T) T, fn(T) T)'
		'maker[T](x T) ?fn (T) T':              '?fn(T) T'
	} {
		for separator in ['\n', '; ', ' '] {
			for following in ['__global number T = T{}', 'fn following(x T) T',
				'fn (h Host) following(x T) T'] {
				source := '${signature}${separator}${following}\n'
				mut a := flat.FlatAst.new()
				mut file_set := token.FileSet.new()
				a.source_files[1] = file_set.add_file('callback_scope.vh', source.len)
				mut tc := TypeChecker.new(&a)
				tc.source_texts_by_file['callback_scope.vh'] = source
				decl := flat.Node{
					kind:   .fn_decl
					pos:    token.new_pos(1, 0)
					typ:    return_type
					is_mut: true
				}
				inside := signature.last_index('T') or { panic('missing return T') }
				outside := source.last_index('T') or { panic('missing following T') }
				assert tc.vls_decl_contains_offset(decl, inside), source
				assert !tc.vls_decl_contains_offset(decl, outside), source
			}
		}
	}
}

// The node of a method is at its name: the receiver before it declares type
// parameters of the method too, so it belongs to the declaration.
fn test_a_method_declaration_starts_at_its_receiver() {
	for source, name in {
		'fn (b Box[T]) get() T {\n\treturn b.value\n}\n':       'get'
		'pub fn (mut b Box[T]) set(value T) {\n}\n':            'set'
		'fn (b Box[fn (T) T]) call(x T) T {\n\treturn x\n}\n':  'call'
		'fn (a Vec[T]) + (b Vec[T]) Vec[T] {\n\treturn a\n}\n': '+'
	} {
		mut a := flat.FlatAst.new()
		mut file_set := token.FileSet.new()
		a.source_files[1] = file_set.add_file('receiver.v', source.len)
		name_at := (source.index(') ${name}') or { panic('missing ${name}') }) + 2
		decl := a.add_node(flat.Node{
			kind: .fn_decl
			pos:  token.new_pos(1, name_at)
		})
		mut tc := TypeChecker.new(&a)
		tc.source_texts_by_file['receiver.v'] = source
		receiver := source.index('T') or { panic('missing receiver T') }
		found := tc.vls_decl_at(1, receiver) or { flat.NodeId(-1) }
		assert found == decl, source
	}
	// A static method has no receiver: what comes before its name is not its.
	source := 'fn Box.new[T](value T) Box[T] {\n\treturn Box[T]{}\n}\n'
	mut a := flat.FlatAst.new()
	mut file_set := token.FileSet.new()
	a.source_files[1] = file_set.add_file('static.v', source.len)
	a.add_node(flat.Node{
		kind: .fn_decl
		pos:  token.new_pos(1, source.index('new') or { panic('missing new') })
	})
	mut tc := TypeChecker.new(&a)
	tc.source_texts_by_file['static.v'] = source
	found := tc.vls_decl_at(1, source.index('Box') or { panic('missing Box') }) or {
		flat.NodeId(-1)
	}
	assert found == flat.NodeId(-1)
}

fn test_a_header_method_on_the_line_of_another_declaration_keeps_its_receiver() {
	source := 'fn first[U](x U) U; fn (b Box[T]) get() T\n'
	mut a := flat.FlatAst.new()
	mut file_set := token.FileSet.new()
	a.source_files[1] = file_set.add_file('same_line.vh', source.len)
	first := a.add_node(flat.Node{
		kind:   .fn_decl
		pos:    token.new_pos(1, source.index('first') or { panic('missing first') })
		is_mut: true
	})
	get := a.add_node(flat.Node{
		kind:   .fn_decl
		pos:    token.new_pos(1, source.index('get') or { panic('missing get') })
		is_mut: true
	})
	mut tc := TypeChecker.new(&a)
	tc.source_texts_by_file['same_line.vh'] = source
	in_first := (source.index('x U') or { panic('missing x U') }) + 2
	in_receiver := (source.index('[T]') or { panic('missing [T]') }) + 1
	assert (tc.vls_decl_at(1, in_first) or { flat.NodeId(-1) }) == first
	assert (tc.vls_decl_at(1, in_receiver) or { flat.NodeId(-1) }) == get
}
