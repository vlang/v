module wit

fn expect_validate_error(src string, code string) {
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains(code), 'expected ${code}, got: ${err.msg()}'
		return
	}
	panic('expected ${code}, validation passed')
}

fn test_validate_version_incompatible() {
	expect_validate_error('package p:q;

@since(version = 0.2.0)
interface a {
	x: func() -> u32;
}

@since(version = 0.1.0)
interface b {
	use a.{x};
	y: func() -> u32;
}
', 'error_version_incompatible')
}

fn test_validate_version_ok() {
	src := 'package p:q;

@since(version = 0.2.0)
interface a {
	x: func() -> u32;
}

@since(version = 0.3.0)
interface b {
	use a.{x};
	y: func() -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or { panic(err) }
}

fn test_validate_unknown_interface_use() {
	expect_validate_error('package p:q;

interface b {
	use nope.{x};
}
', 'error_unknown_interface')
}

fn test_validate_unresolved_use() {
	expect_validate_error('package p:q;

interface base {
	x: func() -> u32;
}

interface derived {
	use base.{missing};
}
', 'error_unresolved_use')
}

fn test_validate_not_a_resource() {
	expect_validate_error('package p:q;

interface i {
	take: func(h: own<handle>) -> u32;
}
', 'error_not_a_resource')
}

fn test_validate_duplicate_constructor() {
	expect_validate_error('package p:q;

interface i {
	resource r {
		constructor(a: u32);
		constructor(b: u32);
	}
}
', 'error_duplicate_constructor')
}

fn test_validate_static_takes_resource() {
	expect_validate_error('package p:q;

interface i {
	resource r {
		make: static func(x: r) -> u32;
	}
}
', 'error_static_method_takes_resource')
}

fn test_validate_containers_ok() {
	src := 'package p:q;

interface i {
	resource handle {
		constructor();
	}
	a: func() -> future<string>;
	b: func() -> stream<u8>;
	m: func() -> map<string, u32>;
	t: func(h: own<handle>) -> borrow<handle>;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or { panic(err) }
}

fn test_validate_unknown_include() {
	expect_validate_error('package p:q;

world base {
	import x: func() -> u32;
}

world derived {
	include missing;
}
', 'error_unknown_include')
}

fn test_validate_unresolved_include_item() {
	expect_validate_error('package p:q;

world base {
	import x: func() -> u32;
}

world derived {
	include base with { nope as renamed };
}
', 'error_unresolved_include')
}

fn test_validate_bad_package_version() {
	expect_validate_error('package p:q@1.0;

interface i {
	f: func() -> u32;
}
', 'error_invalid_package_version')
}

fn test_validate_multi_interface_ok() {
	src2 := 'package example:demo@0.1.0;

@since(version = 0.1.0)
interface types {
	resource handle {
		constructor();
		read: func() -> string;
		make: static func() -> u32;
	}
	take: func(h: own<handle>) -> u32;
}

@since(version = 0.1.0)
interface api {
	use types.{take};
	go: func() -> u32;
}

world app {
	export api;
}
'
	doc := parse(src2) or { panic(err) }
	validate(doc) or { panic(err) }
}
