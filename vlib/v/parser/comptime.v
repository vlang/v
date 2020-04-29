// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.pref
import v.vmod

const (
	supported_platforms = ['windows', 'mac', 'macos', 'darwin', 'linux', 'freebsd', 'openbsd',
		'netbsd', 'dragonfly', 'android', 'js', 'solaris', 'haiku', 'linux_or_macos']
)

// // #include, #flag, #v
fn (mut p Parser) hash() ast.HashStmt {
	val := p.tok.lit
	p.next()
	if val.starts_with('flag') {
		// #flag linux -lm
		mut flag := val[5..]
		// expand `@VROOT` to its absolute path
		if flag.contains('@VROOT') {
			vmod_file_location := vmod.mod_file_cacher.get(p.file_name_dir)
			if vmod_file_location.vmod_file.len == 0 {
				// There was no actual v.mod file found.
				p.error('To use @VROOT, you need' + ' to have a "v.mod" file in ${p.file_name_dir},' +
					' or in one of its parent folders.')
			}
			flag = flag.replace('@VROOT', vmod_file_location.vmod_folder)
		}
		for deprecated in ['@VMOD', '@VMODULE', '@VPATH', '@VLIB_PATH'] {
			if flag.contains(deprecated) {
				p.error('${deprecated} had been deprecated, use @VROOT instead.')
			}
		}
		// println('adding flag "$flag"')
		p.table.parse_cflag(flag, p.mod, p.pref.compile_defines_all) or {
			p.error(err)
		}
		/*
		words := val.split(' ')
		if words.len > 1 && words[1] in supported_platforms {
			if p.pref.os == .mac && words[1] == 'darwin' {
				p.pref.cflags += val.after('darwin')
			}
		}
		*/
	}
	return ast.HashStmt{
		val: val
		mod: p.mod
	}
}

fn (mut p Parser) comp_if() ast.CompIf {
	pos := p.tok.position()
	p.next()
	p.check(.key_if)
	is_not := p.tok.kind == .not
	if is_not {
		p.next()
	}
	val := p.check_name()
	mut stmts := []ast.Stmt{}
	mut skip_os := false
	if val in supported_platforms {
		os := os_from_string(val)
		// `$if os {` for a different target, skip everything inside
		// to avoid compilation errors (like including <windows.h> or calling WinAPI fns
		// on non-Windows systems)
		if !p.pref.is_fmt && ((!is_not && os != p.pref.os) || (is_not && os == p.pref.os)) &&
			!p.pref.output_cross_c {
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
				} else if p.tok.kind == .rcbr {
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
	mut is_opt := false
	if p.tok.kind == .question {
		p.next()
		is_opt = true
	}
	if !skip_os {
		stmts = p.parse_block()
	}
	mut node := ast.CompIf{
		is_not: is_not
		is_opt: is_opt
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

// TODO import warning bug
const (
	todo_delete_me = pref.OS.linux
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
