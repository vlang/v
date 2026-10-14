import os

fn test_optional_interface_owned_assignment_preserves_value_after_return() {
	root := os.join_path(os.vtmp_dir(), 'optional_interface_owned_assignment_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '
interface Named {
	name string
	number() int
}

struct Real {
	name string
	n int
}

fn (value Real) number() int {
	return value.n
}

struct Holder {
mut:
	slot ?Named
}

fn make_value(prefix string) ?Real {
	return Real{name: prefix + ":dynamic", n: 42}
}

fn wrap(prefix string) Holder {
	mut holder := Holder{}
	value := make_value(prefix)
	holder.slot = value
	return holder
}

fn check(holder Holder, expected string) {
	slot := holder.slot or { panic("missing value") }
	assert slot.name == expected
	assert slot.number() == 42
}

fn main() {
	mut holder := wrap("content")
	check(holder, "content:dynamic")
	holder.slot = make_value("replacement")
	check(holder, "replacement:dynamic")
	holder.slot = none
	assert holder.slot == none
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -ownership -gc none -cstrict -cc clang run ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
}
