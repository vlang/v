module parser

import (
	v.ast
	v.pref
)

const (
	supported_platforms = ['windows', 'mac', 'macos', 'darwin', 'linux', 'freebsd', 'openbsd', 'netbsd',
	'dragonfly', 'android', 'js', 'solaris', 'haiku', 'linux_or_macos']
)

fn (p mut Parser) comp_if() ast.CompIf {
	pos := p.tok.position()
	p.next()
	p.check(.key_if)
	is_not := p.tok.kind == .not
	if is_not {
		p.next()
	}
	val := p.check_name()
	mut stmts := []ast.Stmt
	mut skip_os := false
	if val in supported_platforms {
		os := os_from_string(val)
		// `$if os {` for a different target, skip everything inside
		// to avoid compilation errors (like including <windows.h> or calling WinAPI fns
		// on non-Windows systems)
		if false && ((!is_not && os != p.pref.os) || (is_not && os == p.pref.os)) && !p.pref.output_cross_c {
			skip_os = true
			p.check(.lcbr)
			// p.warn('skipping $if $val os=$os p.pref.os=$p.pref.os')
			mut stack := 1
			for {
				if p.tok.kind == .key_return {
					p.returns = true
				}
				if p.tok.kind == .lcbr {
					stack++
				}
				else if p.tok.kind == .rcbr {
					stack--
				}
				if p.tok.kind == .eof {
					break
				}
				if stack <= 0 && p.tok.kind == .rcbr {
					// p.warn('exiting $stack')
					p.next()
					break
				}
				p.next()
			}
		}
	}
	if p.tok.kind == .question {
		p.next()
	}
	if !skip_os {
		stmts = p.parse_block()
	}
	mut node := ast.CompIf{
		is_not: is_not
		pos: pos
		val: val
		stmts: stmts
	}
	if p.tok.kind == .dollar && p.peek_tok.kind == .key_else {
		p.next()
		p.check(.key_else)
		node.has_else = true
		node.else_stmts = p.parse_block()
	}
	return node
}

const (
	todo_delete_me = pref.OS.linux // TODO import warning bug
)

fn os_from_string(os string) pref.OS {
	match os {
		'linux' {
			return .linux
		}
		'windows' {
			return .windows
		}
		'mac' {
			return .mac
		}
		'macos' {
			return .mac
		}
		'freebsd' {
			return .freebsd
		}
		'openbsd' {
			return .openbsd
		}
		'netbsd' {
			return .netbsd
		}
		'dragonfly' {
			return .dragonfly
		}
		'js' {
			return .js
		}
		'solaris' {
			return .solaris
		}
		'android' {
			return .android
		}
		'msvc' {
			// notice that `-os msvc` became `-cc msvc`
			verror('use the flag `-cc msvc` to build using msvc')
		}
		'haiku' {
			return .haiku
		}
		'linux_or_macos' {
			return .linux
		}
		else {
			panic('bad os $os')
		}
	}
	// println('bad os $os') // todo panic?
	return .linux
}
