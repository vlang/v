import os

const issues_vexe = @VEXE
const issues_v3_dir = os.dir(os.dir(@FILE))
const issues_vlib_dir = os.dir(issues_v3_dir)
const issues_v3_src = os.join_path(issues_v3_dir, 'v3.v')

fn issues_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_issues_27981_27984_${os.getpid()}')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${issues_vexe} -gc none -path "${issues_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${issues_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn issues_compile_and_run(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.write_file(src, source) or { panic(err) }
	compile := os.execute('${v3_bin} -nocache ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_generic_closure_returning_struct_with_ierror_field() {
	v3_bin := issues_build_v3()
	output := issues_compile_and_run(v3_bin, 'issue_27984', '
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
	v3_bin := issues_build_v3()
	output := issues_compile_and_run(v3_bin, 'issue_27981', '
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
