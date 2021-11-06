module builtin

// used to generate JS throw statements.

[noreturn]
pub fn js_throw(s any) {
	#throw s

	for {}
}

#let globalPrint;
$if js_freestanding {
	#globalPrint = globalThis.print
}

pub fn println(s string) {
	$if js_freestanding {
		#globalPrint(s.str)
	} $else {
		#console.log(s.str)
	}
}

pub fn print(s string) {
	$if js_node {
		#$process.stdout.write(s.str)
	} $else {
		panic('Cannot `print` in a browser, use `println` instead')
	}
}

pub fn eprintln(s string) {
	$if js_freestanding {
		#globalPrint(s.str)
	} $else {
		#console.error(s.str)
	}
}

pub fn eprint(s string) {
	$if js_node {
		#$process.stderr.write(s.str)
	} $else {
		panic('Cannot `eprint` in a browser, use `println` instead')
	}
}

// Exits the process in node, and halts execution in the browser
// because `process.exit` is undefined. Workaround for not having
// a 'real' way to exit in the browser.
[noreturn]
pub fn exit(c int) {
	JS.process.exit(c)
	js_throw('exit($c)')
}

fn opt_ok(data voidptr, option Option) {
	#option.state = 0
	#option.err = none__
	#option.data = data
}

pub fn unwrap(opt string) string {
	mut o := Option{}
	#o = opt
	if o.state != 0 {
		js_throw(o.err)
	}

	mut res := ''
	#res = opt.data

	return res
}

fn js_stacktrace() string {
	stacktrace := ''
	#let err = new TypeError();
	#err.name = 'stacktrace: '
	#stacktrace.str = err.stack

	return stacktrace
}

pub fn print_backtrace() {
	println(js_stacktrace())
}

// Check for nil value
pub fn isnil(val voidptr) bool {
	res := false
	// This one is kinda weird. In C and native backend we can cast booleans and integers to pointers
	// so we just check *for* all possible NULL-like values here.
	#val = val.valueOf()
	#res.val = val === null || val === undefined || val === false || val === 0 || val === BigInt(0)

	return res
}

pub fn (f float_literal) str() string {
	res := ''
	#res.str += f.valueOf()

	return res
}

pub fn tof64(n JS.Number) f64 {
	res := f64(0.0)
	#res.val = n;

	return res
}

pub fn tof32(n JS.Number) f32 {
	res := f32(0.0)
	#res.val = n;

	return res
}

pub fn toi(n JS.Number) int {
	res := int(0)
	#res.val = Math.floor(n);

	return res
}

pub fn f64tonum(n f64) JS.Number {
	mut res := JS.Number{}
	#res = n.val;

	return res
}

pub fn itonum(n int) JS.Number {
	mut res := JS.Number{}
	#res = n.val;

	return res
}

pub fn i64tobigint(n i64) JS.BigInt {
	mut res := JS.BigInt{}
	#res = n.val;

	return res
}

pub fn u64tobigint(n u64) JS.BigInt {
	mut res := JS.BigInt{}
	#res = n.val;

	return res
}

pub fn tobool(b JS.Boolean) bool {
	res := false
	#res.val = b;

	return res
}

pub fn booltojs(b bool) JS.Boolean {
	mut res := JS.Boolean{}
	#res = b.val;

	return res
}
