import os

struct WrappedReceiverReturnCase {
	typ        string
	expression string
	binding    string
}

fn test_ownership_wrapped_receiver_references_cannot_escape_local_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_wrapped_receiver_escape_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	cases := [
		WrappedReceiverReturnCase{'?&Builder', 'builder.set(42)', ''},
		WrappedReceiverReturnCase{'!&Builder', 'builder.set(42)', ''},
		WrappedReceiverReturnCase{'(int, &Builder)', '42, builder.set(42)', ''},
		WrappedReceiverReturnCase{'!(int, &Builder)', '42, builder.set(42)', ''},
		WrappedReceiverReturnCase{'Holder', 'Holder{target: builder.set(42)}', ''},
		WrappedReceiverReturnCase{'?Holder', 'Holder{target: builder.set(42)}', ''},
		WrappedReceiverReturnCase{'!Holder', 'Holder{target: builder.set(42)}', ''},
		WrappedReceiverReturnCase{'Outer', 'Outer{holder: Holder{target: builder.set(42)}}', ''},
		WrappedReceiverReturnCase{'[]&Builder', '[builder.set(42)]', ''},
		WrappedReceiverReturnCase{'[1]&Builder', '[builder.set(42)]!', ''},
		WrappedReceiverReturnCase{'map[string]&Builder', "{'local': builder.set(42)}", ''},
		WrappedReceiverReturnCase{'Holder', 'holder', 'holder := Holder{target: builder.set(42)}'},
		WrappedReceiverReturnCase{'Holder', 'Holder{...holder, value: 1}', 'holder := Holder{target: builder.set(42)}'},
		WrappedReceiverReturnCase{'Holder', 'holder', 'mut heap := &Builder{drops: drops}; mut holder := Holder{target: heap.set(0)}; if *drops == 0 { holder = Holder{target: builder.set(42)} }'},
		WrappedReceiverReturnCase{'[]&Builder', 'references', 'references := [builder.set(42)]'},
		WrappedReceiverReturnCase{'Holder', 'make_holder(mut builder)', ''},
		WrappedReceiverReturnCase{'Holder', 'identity_holder(Holder{target: builder.set(42)})', ''},
		WrappedReceiverReturnCase{'Holder', 'identity_holder(holder)', 'holder := Holder{target: builder.set(42)}'},
		WrappedReceiverReturnCase{'?&Builder', 'optional_reference(builder.set(42))', ''},
		WrappedReceiverReturnCase{'!&Builder', 'result_reference(builder.set(42))', ''},
		WrappedReceiverReturnCase{'?&Builder', 'optional', 'optional := optional_reference(builder.set(42))'},
		WrappedReceiverReturnCase{'!&Builder', 'result', 'result := result_reference(builder.set(42))'},
		WrappedReceiverReturnCase{'Holder', 'if true { Holder{target: builder.set(42)} } else { Holder{target: builder.set(41)} }', ''},
		WrappedReceiverReturnCase{'Holder', 'if true { mut scoped := Builder{drops: drops}; Holder{target: scoped.set(42)} } else { Holder{target: builder.set(41)} }', ''},
		WrappedReceiverReturnCase{'Holder', 'unsafe { Holder{target: builder.set(42)} }', ''},
		WrappedReceiverReturnCase{'&Holder', '&Holder{target: builder.set(42)}', ''},
		WrappedReceiverReturnCase{'&Builder', 'identity_holder(Holder{target: builder.set(42)}).target', ''},
		WrappedReceiverReturnCase{'&Builder', 'identity_holder(holder).target', 'holder := Holder{target: builder.set(42)}'},
		WrappedReceiverReturnCase{'Outer', 'Outer{...make_outer(mut builder), value: 1}', ''},
		WrappedReceiverReturnCase{'&Holder', 'make_heap_holder(mut builder)', ''},
		WrappedReceiverReturnCase{'&[]&Builder', 'retain_references([builder.set(42)]!)', ''},
		WrappedReceiverReturnCase{'&[]&Builder', 'retain_references(references)', 'references := [builder.set(42)]!'},
		WrappedReceiverReturnCase{'&[]&Builder', 'retain_references(references["row"])', 'references := {"row": [builder.set(42)]!}'},
	]
	for case in cases {
		call := if case.typ == '(int, &Builder)' {
			'_, _ := escaped(&drops)'
		} else if case.typ == '!(int, &Builder)' {
			'_, _ := escaped(&drops) or { panic(err) }'
		} else {
			'_ = escaped(&drops)'
		}
		os.write_file(source, 'struct Builder implements Drop {
mut:
	value int
	drops &int
}
struct Holder { target &Builder value int }
struct Outer { holder Holder value int }
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
fn (mut builder Builder) drop() { unsafe { *builder.drops += 1 } }
fn make_holder(mut builder Builder) Holder { return Holder{target: builder.set(44)} }
fn make_outer(mut builder Builder) Outer { return Outer{holder: make_holder(mut builder)} }
fn make_heap_holder(mut builder Builder) &Holder { return &Holder{target: builder.set(44)} }
fn identity_holder(holder Holder) Holder { return holder }
fn optional_reference(builder &Builder) ?&Builder { return builder }
fn result_reference(builder &Builder) !&Builder { return builder }
fn retain_references(references &[]&Builder) &[]&Builder { return references }
fn escaped(drops &int) ${case.typ} {
	mut builder := Builder{drops: drops}
	${case.binding}
	return ${case.expression}
}
fn main() { mut drops := 0; ${call} }
')!
		for mode in ['-no-parallel', ''] {
			// Rejected fixtures are checked only: their returned pointers would be dangling.
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership ${mode} -check ${os.quoted_path(source)}')
			assert out.exit_code != 0, '${case.typ}: ${case.expression}: ${out.output}'
			assert out.output.contains('cannot return a reference to local storage'), '${case.typ}: ${case.expression}: ${out.output}'
		}
	}
}

fn test_ownership_wrapped_receiver_references_keep_caller_and_heap_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_wrapped_receiver_safe_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Builder { mut: value int }
struct Holder { target &Builder value int }
struct Outer { holder Holder value int }
struct TextHolder { text string }
fn (mut builder Builder) set(value int) &Builder { builder.value = value; return builder }
fn optional_reference(builder &Builder) ?&Builder { return builder }
fn result_reference(builder &Builder) !&Builder { return builder }
fn make_holder(mut builder Builder) Holder { return Holder{target: builder.set(44)} }
fn make_outer(mut builder Builder) Outer { return Outer{holder: make_holder(mut builder)} }
fn make_heap_holder(mut builder Builder) &Holder { return &Holder{target: builder.set(44)} }
fn identity_holder(holder Holder) Holder { return holder }
fn identity_value(builder Builder) Builder { return builder }
fn caller_option(mut builder Builder) ?&Builder { return builder.set(42) }
fn caller_result(mut builder Builder) !&Builder { return builder.set(43) }
fn caller_holder(mut builder Builder) Holder { return make_holder(mut builder) }
fn caller_outer(mut builder Builder) Outer { return Outer{holder: Holder{target: builder.set(45)}} }
fn caller_array(mut builder Builder) []&Builder { return [builder.set(46)] }
fn caller_map(mut builder Builder) map[string]&Builder { return {"caller": builder.set(47)} }
fn caller_pair(mut builder Builder) (int, &Builder) { return 52, builder.set(52) }
fn heap_pair() !(int, &Builder) { mut builder := &Builder{}; return 53, builder.set(53) }
fn heap_option() ?&Builder { mut builder := &Builder{}; return optional_reference(builder.set(48)) }
fn heap_result() !&Builder { mut builder := &Builder{}; return result_reference(builder.set(49)) }
fn heap_holder() Holder {
	mut builder := &Builder{}
	holder := Holder{target: builder.set(50)}
	return identity_holder(holder)
}
fn heap_wrapped_holder() &Holder {
	mut builder := &Builder{}
	return &Holder{target: builder.set(54)}
}
fn heap_projection() &Builder {
	mut builder := &Builder{}
	return identity_holder(Holder{target: builder.set(55)}).target
}
fn replaced_local_reference() Holder {
	mut local := Builder{}
	mut heap := &Builder{}
	holder := Holder{target: local.set(0)}
	return Holder{...holder, target: heap.set(56)}
}
fn rebound_local_reference() Holder {
	mut local := Builder{}
	mut heap := &Builder{}
	mut holder := Holder{target: local.set(0)}
	holder = Holder{target: heap.set(57)}
	return holder
}
fn heap_inherited_projection() Outer {
	mut heap := &Builder{}
	return Outer{...make_outer(mut heap), value: 1}
}
fn heap_forwarded_wrapper() &Holder {
	mut heap := &Builder{}
	return make_heap_holder(mut heap)
}
fn local_value() Builder { builder := Builder{value: 51}; return identity_value(builder) }
fn local_text() TextHolder { text := "keep me".clone(); return TextHolder{text: text} }
fn main() {
	mut builder := Builder{}
	assert voidptr(caller_option(mut builder) or { panic("none") }) == voidptr(&builder)
	assert builder.value == 42
	assert voidptr(caller_result(mut builder) or { panic(err) }) == voidptr(&builder)
	assert builder.value == 43
	assert voidptr(caller_holder(mut builder).target) == voidptr(&builder)
	assert caller_outer(mut builder).holder.target.value == 45
	assert caller_array(mut builder)[0].value == 46
	assert (caller_map(mut builder)["caller"] or { panic("missing") }).value == 47
	assert (heap_option() or { panic("none") }).value == 48
	assert (heap_result() or { panic(err) }).value == 49
	assert heap_holder().target.value == 50
	assert heap_wrapped_holder().target.value == 54
	assert heap_projection().value == 55
	assert replaced_local_reference().target.value == 56
	assert rebound_local_reference().target.value == 57
	assert heap_inherited_projection().holder.target.value == 44
	assert heap_forwarded_wrapper().target.value == 44
	value, reference := caller_pair(mut builder)
	assert value == 52 && reference.value == 52
	heap_value, heap_reference := heap_pair() or { panic(err) }
	assert heap_value == 53 && heap_reference.value == 53
	assert local_value().value == 51
	assert local_text().text == "keep me"
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
	}
}
