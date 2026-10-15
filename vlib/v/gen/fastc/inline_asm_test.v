module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_inline_asm_operand_lowering() {
	aliases := {
		'value':   'u64'
		'address': 'voidptr'
		'narrow':  'u32'
	}
	assert fastc_asm_lower_template('lock xadd value, rax', 'amd64', aliases, true, false, []) == 'lock xadd %%rax, %[value]'
	assert fastc_asm_lower_template('mov value, gs:[0]', 'amd64', aliases, true, false, []) == 'mov %%gs:0, %[value]'
	assert fastc_asm_lower_template('jmp b1', 'amd64', aliases, true, false, []) == 'jmp 1b'
	assert fastc_asm_lower_template('call address', 'amd64', aliases, true, false, []) == 'call *%[address]'
	assert fastc_asm_lower_template('ldr narrow, [address]', 'arm64', aliases, true, false, []) == 'ldr %w[narrow], [%[address]]'
	assert fastc_asm_lower_template('add value, value, 2', 'arm64', aliases, true, false, []) == 'add %[value], %[value], #2'
	assert fastc_asm_lower_template('jmp next', 'amd64', aliases, true, false, ['next']) == 'jmp %l[next]'
	operands := fastc_asm_operands('+m (*target) as address\n=@ccz (ok)\n+r (value)', true) or { panic(err) }
	assert operands.len == 3
	assert operands[0].constraint == '+m'
	assert operands[0].expression == '*target'
	assert operands[0].alias == 'address'
	assert operands[1].constraint == '=@ccz'
	assert fastc_asm_sections(fastc_asm_strip_comments('nop /* ; */\n; ; ; memory')).len == 4
}

fn test_fastc_inline_asm_generated_code_runs_and_cross_compiles() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_inline_asm_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for arch in ['amd64', 'arm64'] {
		mut prefs := pref.new_preferences()
		prefs.target = pref.target_from($if macos { 'macos' } $else { 'linux' }, arch) or { panic(err) }
		source := if arch == 'amd64' {
			r'module main
fn asm_value() u64 {
	mut value := u64(40)
	mut added := u64(2)
	mut carry := false
	target := &value
	asm volatile amd64 {
		lock xadd target, added
		; +m (*target) as target
		  +r (added)
		  =@ccc (carry)
		; ; memory
	}
	asm volatile amd64 {
		jmp f1
		nop
		1:
		; ; ; memory
	}
	asm volatile amd64 raw {
		"addq $0, %[value]\n\t"
		; +r (value)
		; ; cc
	}
	asm volatile amd64 intel {
		add value, 0
		; +r (value)
		; ; cc
	}
	asm goto amd64 {
		jmp done
		; ; ; ; done
	}
	value = 0
	done:
	return if carry { u64(0) } else { value }
}
fn main() { println(asm_value()) }
'
		} else {
			r'module main
fn asm_value() u64 {
	mut value := u64(40)
	mut narrow := u32(0)
	address := &value
	asm volatile aarch64 {
		ldr value, [address]
		add value, value, 2
		mov narrow, 7
		; +r (value)
		  =r (narrow)
		; r (address)
		; memory
	}
	asm volatile arm64 raw {
		"add %[value], %[value], #0\n\t"
		; +r (value)
		; ; memory
	}
	asm goto arm64 {
		b done
		; ; ; ; done
	}
	value = 0
	done:
	return if narrow == 7 { value } else { u64(0) }
}
fn main() { println(asm_value()) }
'
		}
		generated := generate(source, 'inline_asm_${arch}.v', prefs) or { panic(err) }
		body := fastc_test_asm_function(generated, 'asm_value')
		assert body.contains('__asm__ volatile'), body
		c_path := os.join_path(root, '${arch}.c')
		os.write_file(c_path, 'typedef unsigned long long u64;\ntypedef unsigned int u32;\ntypedef _Bool bool;\n#define false 0\n#define true 1\n${body}\nint main(void) { return asm_value() != 42; }\n') or { panic(err) }
		mut flags := ['-x', 'c', '-ffreestanding', '-nostdinc']
		$if macos {
			flags << ['-target',
				if arch == 'amd64' { 'x86_64-apple-darwin' } else { 'arm64-apple-darwin' }]
		} $else {
			flags << ['-target', if arch == 'amd64' {
				'x86_64-unknown-linux-gnu'
			} else {
				'aarch64-unknown-linux-gnu'
			}]
		}
		object := os.join_path(root, '${arch}.o')
		flags << ['-c', c_path, '-o', object]
		compile := cmdexec.run('clang', flags)
		assert compile.exit_code == 0, '${arch}: ${compile.output}\n${body}'
		mut can_run := false
		$if amd64 {
			can_run = arch == 'amd64'
		}
		$if arm64 {
			can_run = arch == 'arm64'
		}
		$if macos && arm64 {
			if arch == 'amd64' {
				// Exercise the x86 fixture too when Rosetta is already available.
				probe := cmdexec.run('arch', ['-x86_64', '/usr/bin/true'])
				can_run = probe.exit_code == 0
			}
		}
		if !can_run { continue }
		program := os.join_path(root, 'program')
		mut link_args := [c_path, '-o', program]
		$if macos {
			link_args << ['-target',
				if arch == 'amd64' { 'x86_64-apple-darwin' } else { 'arm64-apple-darwin' }]
		}
		link := cmdexec.run('clang', link_args)
		assert link.exit_code == 0, link.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}

// Keep the actual FastC-generated body and compile it without host headers so
// the alternate architecture is also checked by Clang's assembler.
fn fastc_test_asm_function(source string, name string) string {
	start := source.last_index('u64 ${name}(') or { panic('missing generated ${name}') }
	open := source.index_after('{', start) or { panic('missing generated function body') }
	mut depth := 0
	mut quote := u8(0)
	mut i := open
	for i < source.len {
		c := source[i]
		if quote != 0 {
			if c == `\\` {
				i += 2
				continue
			}
			if c == quote { quote = 0 }
		} else if c in [`'`, `"`] {
			quote = c
		} else if c == `{` {
			depth++
		} else if c == `}` {
			depth--
			if depth == 0 { return source[start..i + 1] }
		}
		i++
	}
	panic('unfinished generated function body')
}
