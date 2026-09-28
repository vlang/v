import os

const generic_callback_vexe = @VEXE

struct GenericMethodCallbackFoo {}

fn (mut f GenericMethodCallbackFoo) value() int {
	return 42
}

fn (mut f GenericMethodCallbackFoo) call[T](run fn (mut GenericMethodCallbackFoo) T) T {
	return run(mut f)
}

fn test_unbound_instance_method_infers_generic_return_type() {
	mut f := GenericMethodCallbackFoo{}
	assert f.call(GenericMethodCallbackFoo.value) == 42
	assert f.call((GenericMethodCallbackFoo.value)) == 42
}

fn test_private_unbound_instance_method_is_rejected() {
	dir := os.join_path(os.vtmp_dir(), 'generic_callback_private_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'other')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), "Module {\n\tname: 'generic_callback_private'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'other', 'other.v'), 'module other\n\npub struct Foo {}\n\nfn (mut f Foo) value() int {\n\treturn 42\n}\n\npub fn (mut f Foo) call[T](run fn (mut Foo) T) T {\n\treturn run(mut f)\n}\n') or {
		panic(err)
	}
	main_file := os.join_path(dir, 'main.v')
	os.write_file(main_file, 'module main\n\nimport other { Foo }\n\nfn main() {\n\tmut f := Foo{}\n\tprintln(f.call(Foo.value))\n}\n') or {
		panic(err)
	}
	result := os.execute('${os.quoted_path(generic_callback_vexe)} -gc none -o ${os.quoted_path(os.join_path(dir, 'program'))} ${os.quoted_path(main_file)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('method `other.Foo.value` is private'), result.output
}
