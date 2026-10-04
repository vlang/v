import os

fn test_ownership_local_receiver_cannot_escape_in_wrapped_or_aggregate_returns() {
	root := os.join_path(os.vtmp_dir(), 'ownership_wrapped_receiver_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for return_type, expression in {
		'?&Builder':     'local.set(42)'
		'!&Builder':     'local.set(42)'
		'Holder':        'Holder{target: local.set(42)}'
		'?Holder':       'Holder{target: local.set(42)}'
		'!Holder':       'Holder{target: local.set(42)}'
		'[]&Builder':    '[local.set(42)]'
		'[]Holder':      '[Holder{target: local.set(42)}]'
		'(int, Holder)': '0, Holder{target: local.set(42)}'
	} {
		os.write_file(source, 'struct Builder implements Drop {
mut:
	value int
}
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
fn (mut builder Builder) drop() {}
struct Holder { target &Builder }
fn escaped() ${return_type} {
	mut local := Builder{}
	return ${expression}
}
fn main() {}
')!
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang -check ${os.quoted_path(source)}')
		assert out.exit_code != 0, '${return_type}: ${out.output}'
		assert out.output.contains('cannot return a reference to local storage `local`'), '${return_type}: ${out.output}'
	}
}

fn test_ownership_wrapped_receiver_can_return_caller_or_heap_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_wrapped_receiver_safe_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Builder { mut: value int }
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
struct Holder { target &Builder }
fn optional(mut builder Builder) ?&Builder { return builder.set(42) }
fn result(mut builder Builder) !&Builder { return builder.set(42) }
fn aggregate(mut builder Builder) Holder { return Holder{target: builder.set(42)} }
fn aggregate_forwarded(mut builder Builder) Holder { return aggregate(mut builder) }
fn tuple_forwarded(mut builder Builder) (int, Holder) {
	return 0, Holder{target: builder.set(42)}
}
fn heap() ?Holder {
	mut builder := &Builder{}
	return Holder{target: builder.set(42)}
}
fn named_caller(mut builder Builder) Holder {
	held := Holder{target: builder.set(42)}
	return held
}
fn named_heap() Holder {
	mut builder := &Builder{}
	held := Holder{target: builder.set(42)}
	return held
}
fn rewrap(held Holder) Holder { return Holder{target: held.target} }
fn forwarded_caller(mut builder Builder) Holder {
	held := Holder{target: builder.set(42)}
	return rewrap(held)
}
fn owned_value() Builder {
	return Builder{value: 42}
}
fn identity(value Builder) Builder { return value }
fn forwarded_owned_value() Builder {
	local := Builder{value: 42}
	return identity(local)
}
fn main() {
	mut builder := Builder{}
	assert (optional(mut builder) or { panic("none") }).value == 42
	assert (result(mut builder) or { panic(err) }).value == 42
	assert aggregate(mut builder).target.value == 42
	assert aggregate_forwarded(mut builder).target.value == 42
	_, held := tuple_forwarded(mut builder)
	assert held.target.value == 42
	assert (heap() or { panic("none") }).target.value == 42
	assert named_caller(mut builder).target.value == 42
	assert named_heap().target.value == 42
	assert forwarded_caller(mut builder).target.value == 42
	assert owned_value().value == 42
	assert forwarded_owned_value().value == 42
}
')!
	out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang run ${os.quoted_path(source)}')
	assert out.exit_code == 0, out.output
}

fn test_ownership_dereferenced_pointer_call_retains_string_source_loan() {
	root := os.join_path(os.vtmp_dir(), 'ownership_pointer_call_view_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for view in ['*pointer(&owner)', '*forward(pointer(&owner))',
		'*(if owner.len > 0 { pointer(&owner) } else { pointer(&owner) })',
		'*(maybe_pointer(&owner) or { &owner })', '*(result_pointer(&owner) or { &owner })',
		'*unwrap(wrap(&owner))'] {
		for action, expected in {
			'owner = "replacement".to_owned()': 'cannot assign to `owner` because it is borrowed'
			'consume(owner)':                   'cannot move `owner` because it is borrowed'
		} {
			os.write_file(source, 'fn pointer(value &string) &string { return value }
fn forward(value &string) &string { return pointer(value) }
fn maybe_pointer(value &string) ?&string { return value }
fn result_pointer(value &string) !&string { return value }
struct Box { value &string }
fn wrap(value &string) Box { return Box{value: value} }
fn unwrap(box Box) &string { return box.value }
fn consume(value string) { _ = value }
fn main() {
	mut owner := "original".to_owned()
	view := ${view}
	${action}
	println(view)
}
')!
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang -check ${os.quoted_path(source)}')
			assert out.exit_code != 0, '${view}: ${out.output}'
			assert out.output.contains(expected), '${view}: ${out.output}'
		}
	}
	os.write_file(source, 'fn pointer(value &string) &string { return value }
fn main() {
	owner := "original".to_owned()
	view := *pointer(&owner)
	assert view == owner
	assert unsafe { view.str == owner.str }
}
')!
	out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang run ${os.quoted_path(source)}')
	assert out.exit_code == 0, out.output
}

fn test_ownership_user_substr_unsafe_method_preserves_owned_return() {
	root := os.join_path(os.vtmp_dir(), 'ownership_user_substr_unsafe_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for method in ['substr_unsafe', 'owned_text'] {
		os.write_file(source, 'struct Source {}
fn (source Source) ${method}() string { return "independent".to_owned() }
fn consume(value string) { _ = value }
fn main() {
	source := Source{}
	value := source.${method}()
	consume(value)
	println(value)
}
')!
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang -check ${os.quoted_path(source)}')
		assert out.exit_code != 0, '${method}: ${out.output}'
		assert out.output.contains('use of moved value: `value`'), '${method}: ${out.output}'
	}
}

fn test_ownership_local_receiver_cannot_escape_through_aggregate_aliases() {
	root := os.join_path(os.vtmp_dir(), 'ownership_aggregate_alias_receiver_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for name, body in {
		'named':       'held := Holder{target: local.set(42)}; return held'
		'forwarded':   'return wrap(mut local)'
		'tuple_call':  'return tuple_wrap(mut local)'
		'nested':      'held := Holder{target: local.set(42)}; return nest(held)'
		'result':      'return local.reference()!'
		'optional':    'return local.optional() or { return none }'
		'conditional': 'return if local.value == 0 { Holder{target: local.set(42)} } else { Holder{target: local.set(41)} }'
	} {
		return_type := match name {
			'tuple_call' { '(int, Holder)' }
			'nested' { 'Outer' }
			'result' { '!&Builder' }
			'optional' { '?&Builder' }
			else { 'Holder' }
		}
		os.write_file(source, 'struct Builder implements Drop { mut: value int }
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
fn (mut builder Builder) drop() {}
struct Holder { target &Builder }
struct Outer { held Holder }
fn nest(held Holder) Outer { return Outer{held: held} }
fn (mut builder Builder) reference() !&Builder { return builder }
fn (mut builder Builder) optional() ?&Builder { return builder }
fn wrap(mut target Builder) Holder { return Holder{target: target.set(42)} }
fn tuple_wrap(mut target Builder) (int, Holder) { return 0, Holder{target: target.set(42)} }
fn escaped() ${return_type} {
	mut local := Builder{}
	${body}
}
fn main() {}
')!
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang -check ${os.quoted_path(source)}')
		assert out.exit_code != 0, '${name}: ${out.output}'
		assert out.output.contains('cannot return a reference to local storage `local`'), '${name}: ${out.output}'
	}
}

fn test_ownership_copied_fallback_detaches_loan_and_retains_owned_result() {
	root := os.join_path(os.vtmp_dir(), 'ownership_copied_fallback_loan_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for creation, text in {
		'*pointer(&original)':                     'fallback'
		'unsafe { original.substr_unsafe(1, 4) }': 'all'
	} {
		for action, expected in {
			'assert result == "${text}"':       ''
			'consume(result); println(result)': 'use of moved value: `result`'
		} {
			os.write_file(source, 'fn consume(value string) { _ = value }
fn pointer(value &string) &string { return value }
fn main() {
	mut original := "fallback".to_owned()
	mut view := ${creation}
	result := "abc".substr_or(0, 4, view)
	assert view == "${text}"
	view = ""
	original = "replacement".to_owned()
	${action}
}
')!
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang -check ${os.quoted_path(source)}')
			if expected.len == 0 {
				assert out.exit_code == 0, out.output
				completed := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -gc none -cc clang run ${os.quoted_path(source)}')
				assert completed.exit_code == 0, completed.output
			} else {
				assert out.exit_code != 0, out.output
				assert out.output.contains(expected), out.output
				assert !out.output.contains('cannot assign to `original`'), out.output
			}
		}
	}
}
