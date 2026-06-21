module c

const c_reserved_words = ['auto', 'break', 'case', 'char', 'const', 'continue', 'copy', 'default',
	'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
	'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch',
	'typedef', 'union', 'unsigned', 'void', 'volatile', 'while']

fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	// The V builtin `exit` wraps `C.exit`; both lower to the C symbol `exit`.
	// Rename the V function (and its call sites) to `v_exit` so its body's
	// `C.exit(code)` call resolves to libc `exit` instead of recursing forever.
	// `C.exit` itself is handled by the `C.` strip above, so it stays `exit`.
	if name == 'exit' {
		return 'v_exit'
	}
	n := name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('&', 'ptr').replace('[', '_').replace(']', '').replace(',', '_').replace(' ', '_').replace('.', '__')
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}

fn c_escape(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t').replace('\r',
		'\\r')
}
