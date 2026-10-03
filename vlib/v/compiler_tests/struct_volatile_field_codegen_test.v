import os

const volatile_field_vexe = @VEXE
const volatile_field_tests_dir = os.dir(@FILE)
const volatile_field_v3_dir = os.dir(volatile_field_tests_dir)
const volatile_field_vlib_dir = os.dir(volatile_field_v3_dir)
const volatile_field_v3_src = os.join_path(volatile_field_v3_dir, 'v.v')

fn volatile_field_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_volatile_field_codegen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.exec([volatile_field_vexe, '-gc', 'none', '-path',
			'${volatile_field_vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			'${volatile_field_v3_src}'])
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn volatile_field_generate_c(v3_bin string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_volatile_field_${os.getpid()}.v')
	c_path := os.join_path(os.temp_dir(), 'v3_volatile_field_${os.getpid()}.c')
	os.write_file(src, source) or { panic(err) }
	defer {
		os.rm(src) or {}
		os.rm(c_path) or {}
	}
	generate := os.exec([v3_bin, '-o', c_path, '${src}'])
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }
}

// A `volatile` struct field must stay volatile in the C declaration. A driver
// polling a device register through one (`for port.regs.ci & bit != 0 {}`)
// otherwise reads the register once, and -prod turns the loop into a jump to
// itself. The checker recorded the qualifier, but the struct table was copied
// without it on its way to C generation, and generic specializations never
// set it.
fn test_volatile_struct_fields_keep_the_qualifier_in_c() {
	v3_bin := volatile_field_build_v3()
	defer {
		os.rm(v3_bin) or {}
	}
	c := volatile_field_generate_c(v3_bin, 'module main

struct Regs {
mut:
	ci u32
}

struct Port {
	name string
	volatile regs &Regs
	volatile status u32
	plain           u32
}

struct Ring[T] {
mut:
	volatile head &T
	tail          int
}

fn wait_ci(port &Port, bit u32) {
	for port.regs.ci & bit != 0 {}
}

fn main() {
	mut regs := Regs{}
	port := Port{
		regs: &regs
	}
	wait_ci(&port, 1)
	mut slot := u16(7)
	ring := Ring[u16]{
		head: &slot
	}
	println(port.status + port.plain + u32(*ring.head))
}
')
	assert c.contains('\tvolatile main__Regs* regs;'), c
	assert c.contains('\tvolatile u32 status;'), c
	assert c.contains('\tu32 plain;'), c
	assert !c.contains('volatile u32 plain;'), c
	assert c.contains('\tvolatile u16* head;'), c
	assert !c.contains('volatile string name;'), c
}
