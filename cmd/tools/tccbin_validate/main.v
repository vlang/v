module main

import os

const command_prefix = '> C compiler cmd: '

struct CommandPaths {
	tcc    string
	libgc  string
	rpath  string
	binary string
}

// The allowlist deliberately excludes shell expansions even inside quotes.
fn tokenize_command(raw string) ![]string {
	for i, ch in raw {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ' \t/._,+-=:\'"'.contains(ch.ascii_str())) {
			return error('raw-character: forbidden raw character U+${ch:04X} at offset ${i}')
		}
	}
	mut tokens := []string{}
	mut current := ''
	mut quote := u8(0)
	mut started := false
	for ch in raw {
		if quote != 0 {
			if ch == quote {
				quote = 0
			} else {
				current += ch.ascii_str()
			}
		} else if ch == `"` || ch == `'` {
			quote = ch
			started = true
		} else if ch == ` ` || ch == `\t` {
			if started {
				tokens << current
				current = ''
				started = false
			}
		} else {
			current += ch.ascii_str()
			started = true
		}
	}
	if quote != 0 {
		return error('tokenization: unclosed quote in -showcc compiler command')
	}
	if started {
		tokens << current
	}
	for token in tokens {
		if token.starts_with('@') {
			return error('response-file: response-file tokens are forbidden')
		}
	}
	return tokens
}

fn validate_showcc(showcc string, paths CommandPaths) ![]string {
	if showcc.contains('\n') || showcc.contains('\r') {
		return error('raw-character: line breaks are forbidden in -showcc output')
	}
	if !showcc.starts_with(command_prefix) || showcc.count(command_prefix) != 1 {
		return error('command-count: expected exactly one standalone -showcc compiler command')
	}
	tokens := tokenize_command(showcc[command_prefix.len..])!
	for token in tokens {
		if token.starts_with('-o') && token != '-o' {
			return error('output-joined: joined TinyCC output options are forbidden')
		}
	}
	if tokens.filter(it == '-o').len != 1 {
		return error('output-arity: expected exactly one -o token')
	}
	output_index := tokens.index('-o')
	if output_index + 1 >= tokens.len || tokens[output_index + 1] != paths.binary {
		return error('output-pair: expected adjacent -o ${paths.binary} tokens')
	}
	if tokens.filter(it == paths.tcc).len != 1 {
		return error('compiler-arity: expected exactly one TCC token ${paths.tcc}')
	}
	if tokens[0] != paths.tcc {
		return error('compiler-position: expected first token ${paths.tcc}')
	}
	rpath := '-Wl,-rpath,${paths.rpath}'
	if tokens.filter(it == paths.libgc).len != 1 || tokens.filter(it == rpath).len != 1 {
		return error('link-contract: expected exactly one libgc and rpath token')
	}
	for token in tokens {
		if (token.starts_with(paths.tcc) && token != paths.tcc)
			|| (token.starts_with(paths.libgc) && token != paths.libgc)
			|| (token.starts_with(rpath) && token != rpath) {
			return error('link-contract: rejected near-match compiler, libgc or rpath token')
		}
		if token.contains('libgc.a') {
			return error('link-contract: static libgc.a token is forbidden')
		}
	}
	return tokens
}

fn main() {
	paths := CommandPaths{
		tcc:    os.getenv('EXPECTED_TCC')
		libgc:  os.getenv('EXPECTED_LIBGC')
		rpath:  os.getenv('EXPECTED_RPATH')
		binary: os.getenv('EXPECTED_BINARY')
	}
	validate_showcc(os.getenv('SHOWCC_OUTPUT'), paths) or {
		eprintln(err)
		exit(1)
	}
	println('confirmed exact compiler token: ${paths.tcc}')
	println('confirmed exact libgc token: ${paths.libgc}')
	println('confirmed exact rpath token: -Wl,-rpath,${paths.rpath}')
	println('confirmed: no static libgc.a token is present')
}
