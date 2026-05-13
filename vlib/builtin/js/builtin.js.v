module builtin

// used to generate JS throw statements.

$if js_node {
	#var $fs = require('fs');
}

@[noreturn]
pub fn js_throw(s any) {
	#throw s

	for {}
}

#let globalPrint, globalWrite;
$if js_freestanding {
	#globalPrint = globalThis.print
	#globalWrite = (typeof globalThis.write === 'function')? write: globalThis.print
}

pub fn flush_stdout() {
	// needed for parity with builtin.c.v
}

pub fn flush_stderr() {
	// needed for parity with builtin.c.v
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
	} $else $if js_freestanding {
		#globalWrite(s.str)
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

// input_character gives back a single character, read from the standard input.
// It returns -1 on error (when the input is finished (EOF), on a broken pipe etc).
pub fn input_character() int {
	$if js_node {
		mut ch := -1
		#try {
		#const read_buffer = Buffer.alloc(1)
		#const nbytes = $fs.readSync(0, read_buffer, 0, 1, null)
		#if (nbytes > 0) { ch.val = read_buffer[0] }
		#} catch (e) {}

		return ch
	} $else {
		return -1
	}
}

// print_character writes the single character `ch` to the standard output.
// It returns the written character value, or panics if the active JS runtime
// does not support stdout writes.
pub fn print_character(ch u8) int {
	print(ch.ascii_str())
	return ch
}

// Exits the process in node, and halts execution in the browser
// because `process.exit` is undefined. Workaround for not having
// a 'real' way to exit in the browser.
@[noreturn]
pub fn exit(c int) {
	JS.process.exit(c)
	js_throw('exit(${c})')
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

// v_clone_value preserves standalone JS semantics for V clone methods.
#function v_clone_value(value) {
#if (value === null || value === undefined) return value;
#if (value instanceof $ref) return new $ref(v_clone_value(value.val));
#if (value instanceof array) return array_clone(value);
#if (value instanceof map) {
#let cloned = {}
#for (const key in value.map) cloned[key] = { key: v_clone_value(value.map[key].key), val: v_clone_value(value.map[key].val) }
#return new map(cloned);
#}
#if (typeof value !== 'object') return value;
#if (typeof value.$toJS === 'function') {
#let cloned = Object.create(Object.getPrototypeOf(value) || Object.prototype);
#for (const key of Object.keys(value)) cloned[key] = v_clone_value(value[key]);
#return cloned;
#}
#let cloned;
#try {
#cloned = typeof value.constructor === 'function' ? new value.constructor({}) : Object.create(Object.getPrototypeOf(value));
#} catch (e) {
#cloned = Object.create(Object.getPrototypeOf(value) || Object.prototype);
#}
#for (const key of Object.keys(value)) cloned[key] = v_clone_value(value[key]);
#return cloned;
#}

pub fn print_backtrace() {
	println(js_stacktrace())
}

pub fn (a array) clone() array {
	mut res := empty_array()
	#const source = a instanceof $ref ? a.val : a
	#const cloned = source.arr.arr.slice(source.arr.index_start.valueOf(), source.arr.index_start.valueOf() + source.len.valueOf()).map(v_clone_value)
	#res = new array(new array_buffer({arr: cloned, len: new int(cloned.length), cap: new int(cloned.length), index_start: new int(0), has_slice: new bool(false)}))

	return res
}

// Check for nil value
pub fn isnil(val voidptr) bool {
	res := false
	// This one is kinda weird. In C and native backend we can cast booleans and integers to pointers
	// so we just check *for* all possible NULL-like values here.
	#if (typeof val == 'function') { res.val = false; } else {
	#val = val instanceof voidptr ? val.valueOf().val : val;
	#res.val = val === null || val === undefined || val === false || val === 0 || val === BigInt(0) || (val instanceof int ? val.val == 0 : false)
	#}

	return res
}

pub fn (f float_literal) str() string {
	res := ''
	#res.str += f.valueOf()

	return res
}
