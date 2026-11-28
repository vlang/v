import os
import v.tempname

fn test_optional_match_and_generic_closure_codegen_regressions() {
	root := os.join_path(os.vtmp_dir(), 'v3_result_codegen_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'app')
	src := os.join_path(root, 'main.v')
	os.write_file(src, '
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

// #27984: a generic closure can return a struct with an IError field.
fn check_generic_closure() {
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

fn convert(value int) !int {
	return value
}

// #27981: an optional binding can propagate a Result inside a match branch.
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
	check_generic_closure()
	println("\${transform(66, true)!},\${transform(none, true)!},\${transform(66, false)!}")
}
') or { panic(err) }
	compile := os.exec([@VEXE, '-nocache', src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['0,fail: boom 1', '66,0,0'], run.output
}
