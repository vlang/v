@[has_globals]
module debug

@[markused]
pub struct FnTrace {
	name string
	file string
	line i64
}

@[markused]
__global g_callstack = []FnTrace{}

@[markused]
pub fn dump_callstack() {
	eprintln('Backtrace:')
	eprintln('${'-':50r}')
	for k, item in g_callstack.reverse() {
		eprintln('${item.file}:${item.line:-4d} | ${'>'.repeat(k + 1)} ${item.name}')
	}
	eprintln('${'-':50r}')
}
