import os
import v.tempname

fn issues_compile_and_run(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, '-nocache', src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_generic_closure_returning_struct_with_ierror_field() {
	output := issues_compile_and_run('issue_27984', '
struct Outcome[T] {
	value T
	err   IError = none
}

fn retry_op[T](op fn () Outcome[T]) Outcome[T] {
	result := op()
	if result.err is none {
		return result
	}
	return Outcome[T]{
		value: result.value
		err:   error("fail: \${result.err.msg()}")
	}
}

fn main() {
	mut count := 0
	operation := fn [mut count] () Outcome[int] {
		count++
		if count == 3 {
			return Outcome[int]{
				value: 42
			}
		}
		return Outcome[int]{
			err: error("boom \${count}")
		}
	}
	result := retry_op(operation)
	println("\${result.value},\${result.err.msg()}")
}
')
	assert output == '0,fail: boom 1'
}

fn test_optional_binding_inside_match_with_result_propagation() {
	output := issues_compile_and_run('issue_27981', '
fn convert(value int) !int {
	return value
}

fn transform(input ?int, flag bool) !int {
	return match flag {
		true {
			if value := input {
				normalized := convert(value)!
				normalized
			} else {
				0
			}
		}
		false {
			0
		}
	}
}

fn main() {
	println("\${transform(66, true)!},\${transform(none, true)!},\${transform(66, false)!}")
}
')
	assert output == '66,0,0'
}
