module fastc

import os
import v.cmdexec

// `$if field.unaliased_typ is bool` inside `$for field in T.fields` tests the field type with
// aliases resolved, as `field.typ is X` does without them; an option field is only `$option`.
fn test_fastc_comptime_field_unaliased_type_dispatch() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_field_unaliased_type_dispatch_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

type Flag = bool

struct Opts {
mut:
	default_     bool
	changes_only bool = true
	count        u32
	name         string
	maybe        ?bool
	flag         Flag
	tags         ?[]string
	inner        Inner
}

struct Inner {
	x int
}

fn set_field[T](mut dst T, key string, value ?string) ! {
	raw := if v := value { v } else { '' }
	has_value := value != none
	$for field in T.fields {
		if field.name == key {
			$if field.unaliased_typ is string {
				dst.$(field.name) = raw
				return
			} $else $if field.unaliased_typ is bool {
				dst.$(field.name) = if has_value { raw == 'true' } else { true }
				return
			} $else $if field.unaliased_typ is ?bool {
				dst.$(field.name) = raw == 'true'
				return
			} $else $if field.unaliased_typ is $struct {
				return error('struct \${field.name}')
			} $else $if field.unaliased_typ is u32 || field.unaliased_typ is ?u32 {
				dst.$(field.name) = u32(raw.int())
				return
			} $else {
				return error('unsupported field type')
			}
		}
	}
	return error('invalid field')
}

fn main() {
	mut o := Opts{}
	set_field(mut o, 'default_', ?string(none)) or { println(err) }
	set_field(mut o, 'changes_only', 'false') or { println(err) }
	set_field(mut o, 'count', '7') or { println(err) }
	set_field(mut o, 'name', 'x') or { println(err) }
	set_field(mut o, 'maybe', 'true') or { println(err) }
	set_field(mut o, 'flag', 'true') or { println(err) }
	set_field(mut o, 'tags', 'a') or { println(err) }
	set_field(mut o, 'inner', 'a') or { println(err) }
	println('\${o.default_} \${o.changes_only} \${o.count} \${o.name} \${o.maybe} \${o.flag}')
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'unsupported field type\nstruct inner\ntrue false 7 x Option(true) true', run.output
}
