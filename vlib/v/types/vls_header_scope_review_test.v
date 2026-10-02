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
