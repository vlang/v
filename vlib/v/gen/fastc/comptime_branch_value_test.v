module fastc

import os
import v.cmdexec

fn comptime_branch_value_expression(branch int, body string, fallback string) string {
	return match branch {
		0 { '\$if true {\n${body}\n} \$else { ${fallback} }' }
		1 { '\$if false { ${fallback} } \$else {\n${body}\n}' }
		else { '\$if false { ${fallback} } \$else \$if true {\n${body}\n} \$else { ${fallback} }' }
	}
}

fn test_comptime_expression_branches_preserve_values_and_cleanup() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_branch_value_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	template := r'module main
struct Value { number int }
struct Observation { mut: value int }
fn add_one(value int) int { return value + 1 }
fn fallible(ok bool) !int {
    if !ok { return error("failed") }
    return 42
}
fn load(ok bool) bool {
    value := STRUCT_BRANCH
    return value.number == 42
}
fn saved_value_with_cleanup(mut observed Observation) int {
    saved := CLEANUP_BRANCH
    return saved
}
fn main() {
    assert load(true)
    assert !load(false)
    optional := OPTION_BRANCH
    if value := optional {
        assert value.number == 42
    } else {
        panic("lost the branch-local option payload")
    }
    nested_number := NUMBER_BRANCH
    assert nested_number == 42
    nested_call := CALL_BRANCH
    assert nested_call == 42
    old_increment := INCREMENT_BRANCH
    assert old_increment == 41
    old_decrement := DECREMENT_BRANCH
    assert old_decrement == 43
    following_value := POSTFIX_STATEMENT_BRANCH
    assert following_value == 42
    $if check_fastc_cleanup ? {
        mut cleanup_observed := Observation{}
        assert saved_value_with_cleanup(mut cleanup_observed) == 42
        assert cleanup_observed.value == 42
    }
}
'
	for branch in 0 .. 3 {
		mut source := template.replace('STRUCT_BRANCH', comptime_branch_value_expression(branch,
			r'number := fallible(ok) or { return false }
Value{number: number}', 'Value{}'))
		source = source.replace('OPTION_BRANCH', comptime_branch_value_expression(branch,
			r'wrapped := ?Value(Value{number: 42})
wrapped', '?Value(none)'))
		source = source.replace('NUMBER_BRANCH', comptime_branch_value_expression(branch,
			r'number := 41
$if true { number + 1 } $else { 0 }', '0'))
		source = source.replace('CALL_BRANCH', comptime_branch_value_expression(branch,
			r'number := 41
$if false { 0 } $else $if true { add_one(number) } $else { 0 }', '0'))
		source = source.replace('INCREMENT_BRANCH', comptime_branch_value_expression(branch,
			r'mut number := 41
number++', '0'))
		source = source.replace('DECREMENT_BRANCH', comptime_branch_value_expression(branch,
			r'mut number := 43
number--', '0'))
		source = source.replace('POSTFIX_STATEMENT_BRANCH', comptime_branch_value_expression(branch,
			r'mut number := 41
number++
number', '0'))
		source = source.replace('CLEANUP_BRANCH', comptime_branch_value_expression(branch,
			r'mut number := 41
$if true { number++ }
defer { observed.value = number; number = 0 }
number', '0'))
		os.write_file(os.join_path(root, 'main.v'), source)!
		for backend in ['c', 'fastc'] {
			program := os.join_path(root, 'program_${branch}_${backend}')
			mut args := ['-b', backend, '-gc', 'none', '-nocache', '-o', program]
			if backend == 'fastc' {
				// The C backend omits this compile-time branch defer.
				args << ['-d', 'check_fastc_cleanup']
			}
			args << root
			build := cmdexec.run(@VEXE, args)
			assert build.exit_code == 0, '${backend}, branch ${branch}: ${build.output}'
			run := cmdexec.run(program, [])
			assert run.exit_code == 0, '${backend}, branch ${branch}: ${run.output}'
		}
	}
}
