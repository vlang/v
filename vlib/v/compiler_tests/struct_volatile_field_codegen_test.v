import os
import v.tempname

fn volatile_field_generate_c(source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_volatile_field_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_volatile_field.v')
	c_path := os.join_path(root, 'v3_volatile_field.c')
	os.write_file(src, source) or { panic(err) }
	generate := os.exec([@VEXE, '-o', c_path, '${src}'])
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
	c := volatile_field_generate_c('module main

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
