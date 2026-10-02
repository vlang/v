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
