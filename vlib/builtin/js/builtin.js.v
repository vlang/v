module builtin

// used to generate JS throw statements.
pub fn js_throw(s any) {
	#throw s
}

fn fmt(s any) string {
	mut res := ''

	#if (Object.getPrototypeOf(s).hasOwnProperty('str') && typeof s.str == 'function') res = s.str()
	#else res.str = s.toString()

	return res
}

pub fn println(s any) {
	$if js_freestanding {
		#print(fmt(s).str)
	} $else {
		#console.log(fmt(s).str)
	}
}

pub fn print(s any) {
	$if js_node {
		#$process.stdout.write(fmt(s).str)
	} $else {
		panic('Cannot `print` in a browser, use `println` instead')
	}
}

pub fn eprintln(s any) {
	$if js_freestanding {
		#print(fmt(s).str)
	} $else {
		#console.error(fmt(s).str)
	}
}

pub fn eprint(s any) {
	$if js_node {
		#$process.stderr.write(fmt(s).str)
	} $else {
		panic('Cannot `eprint` in a browser, use `println` instead')
	}
}

// Exits the process in node, and halts execution in the browser
// because `process.exit` is undefined. Workaround for not having
// a 'real' way to exit in the browser.
pub fn exit(c int) {
	JS.process.exit(c)
	js_throw('exit($c)')
}

fn opt_ok(data voidptr, option Option) {
	#option.state = 0
	#option.err = none__
	#option.data = data
}

pub fn unwrap(opt any) any {
	mut o := Option{}
	#o = opt
	if o.state != 0 {
		js_throw(o.err)
	}
	return opt
}
