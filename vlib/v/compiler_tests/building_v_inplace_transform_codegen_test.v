import os
import v.cmdexec

// The parallel transform of a `-building-v` build rewrites the children of
// source nodes in place, which the serial transform never does. A multi-type
// match arm transforms its body once per variant, so a body statement that
// reassigns the subject (which lifts the smartcast that otherwise blocks the
// in-place rewrite) must not keep the first variant's lowering for the others.
// An array append decides whether to clone its operand from the source
// operand, so lowering `(m[k] or { ... })` must not turn that paren into an
// addressable temp first.
const inplace_transform_program = 'module main

struct PtrT {
	base_type &Ty
}

struct OptT {
	pad       i64
	base_type &Ty
}

struct ResT {
	pad1      i64
	pad2      i64
	base_type &Ty
}

type Ty = PtrT | OptT | ResT | int

fn ty_reference(value Ty) &Ty {
	return &value
}

fn unwrap_stmt(t Ty) int {
	mut typ := t
	for {
		match typ {
			PtrT, OptT, ResT {
				typ = *typ.base_type
			}
			else {
				break
			}
		}
	}
	return if typ is int { typ } else { -1 }
}

fn unwrap_value(t Ty) int {
	mut typ := t
	layers := match typ {
		PtrT, OptT, ResT {
			typ = *typ.base_type
			1
		}
		else {
			0
		}
	}
	return if typ is int { typ * 10 + layers } else { -1 }
}

fn unwrap_return(t Ty) int {
	mut typ := t
	return match typ {
		PtrT, OptT, ResT {
			typ = *typ.base_type
			if typ is int { typ } else { -1 }
		}
		else {
			0
		}
	}
}

struct Local {
	name  string
	items []int
}

fn collect(m map[string]Local, names []string) []Local {
	mut out := []Local{}
	for name in names {
		out << (m[name] or { Local{} })
		out.prepend((m[name] or { Local{} }))
		out.insert(1, (m[name] or { Local{} }))
	}
	return out
}

fn main() {
	values := [Ty(PtrT{
		base_type: ty_reference(Ty(1))
	}), Ty(OptT{
		pad:       100
		base_type: ty_reference(Ty(2))
	}), Ty(ResT{
		pad1:      100
		pad2:      200
		base_type: ty_reference(Ty(3))
	})]
	for value in values {
		println("\${unwrap_stmt(value)} \${unwrap_value(value)} \${unwrap_return(value)}")
	}
	m := {
		"a": Local{
			name:  "x"
			items: [1, 2]
		}
	}
	println(collect(m, ["a", "b"]).len)
}
'

fn first_line_difference(expected string, actual string) string {
	expected_lines := expected.split_into_lines()
	actual_lines := actual.split_into_lines()
	for i, line in expected_lines {
		if i >= actual_lines.len || actual_lines[i] != line {
			got := if i < actual_lines.len { actual_lines[i] } else { '<end>' }
			return 'line ${i + 1}\n- ${line}\n+ ${got}'
		}
	}
	return 'line ${expected_lines.len + 1}\n+ ${actual_lines[expected_lines.len]}'
}

fn test_building_v_parallel_transform_lowers_like_the_serial_one() {
	root := os.join_path(os.vtmp_dir(), 'v3_building_v_inplace_transform_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, inplace_transform_program)!
	mut serial := ''
	for jobs in ['1', '2', '8'] {
		c_file := os.join_path(root, 'main_${jobs}.c')
		generated := cmdexec.run_in_merged_with_env(@VEXE, ['-building-v', '-nocache', '-o', c_file,
			source], root, {
			'VJOBS': jobs
		})
		assert generated.exit_code == 0, 'VJOBS=${jobs}: ${generated.output}'
		code := os.read_file(c_file)!
		if serial.len == 0 {
			serial = code
			continue
		}
		if code != serial {
			assert false, 'VJOBS=${jobs} generated different C than VJOBS=1: ${first_line_difference(serial, code)}'
		}
	}
	mut executable := os.join_path(root, 'main')
	$if windows {
		executable += '.exe'
	}
	compiled := cmdexec.run_in_merged_with_env(@VEXE, ['-building-v', '-nocache', '-o', executable,
		source], root, {
		'VJOBS': '2'
	})
	assert compiled.exit_code == 0, compiled.output
	ran := cmdexec.run_with_timeout(executable, [], 30_000)
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space().split_into_lines() == ['1 11 1', '2 21 2', '3 31 3', '6']
}
