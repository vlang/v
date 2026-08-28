import os

const inline_asm_vexe = @VEXE
const inline_asm_tests_dir = os.dir(@FILE)
const inline_asm_v3_dir = os.dir(inline_asm_tests_dir)
const inline_asm_vlib_dir = os.dir(inline_asm_v3_dir)
const inline_asm_v3_source = os.join_path(inline_asm_v3_dir, 'v3.v')

fn inline_asm_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_inline_asm_${name}_${os.getpid()}')
}

fn build_v3_inline_asm() string {
	v3_bin := inline_asm_tmp_path('compiler')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${inline_asm_vexe} -gc none -path "${inline_asm_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${inline_asm_v3_source}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn inline_asm_runtime_source() string {
	$if arm64 {
		return 'fn main() {
	asm amd64 {
		; ; ; memory
	}
	a := 10
	mut b := 0
	asm arm64 {
		mov x0, a
		mov b, x0
		; +r (b)
		; r (a)
		; x0
	}
	mut loops := 0
	asm arm64 {
		mov x0, 3
		loop_start:
		add loops, loops, 2
		sub x0, x0, 1
		cmp x0, 0
		b.gt loop_start
		; +r (loops)
		; ; x0
	}
	value := 5
	ptr := &value
	asm volatile arm64 {
		mov w0, 7
		str w0, [ptr]
		; ; r (ptr)
		; w0
		  memory
	}
	println(b)
	println(loops)
	println(value)
}
'
	} $else $if amd64 {
		return 'fn main() {
	asm arm64 {
		; ; ; memory
	}
	a := 10
	mut b := 0
	asm amd64 {
		mov rax, a
		mov b, rax
		; +r (b)
		; r (a)
		; rax
	}
	mut loops := 0
	asm amd64 {
		mov rcx, 3
		loop_start:
		add loops, 2
		loop loop_start
		; +r (loops)
		; ; rcx
	}
	value := 5
	ptr := &value
	asm volatile amd64 {
		movq [ptr], 7
		; ; r (ptr)
		; memory
	}
	println(b)
	println(loops)
	println(value)
}
'
	} $else {
		return ''
	}
}

fn test_inline_asm_c_lowering_preserves_named_operands_and_runs() {
	source := inline_asm_runtime_source()
	if source.len == 0 {
		return
	}
	v3_bin := build_v3_inline_asm()
	source_path := '${inline_asm_tmp_path('program')}.v'
	c_path := '${inline_asm_tmp_path('program')}.c'
	bin_path := inline_asm_tmp_path('program')
	os.write_file(source_path, source) or { panic(err) }
	generate := os.execute('${v3_bin} -cc clang -o ${c_path} ${source_path}')
	assert generate.exit_code == 0, generate.output
	assert !generate.output.contains('inline assembly is not supported'), generate.output
	c_source := os.read_file(c_path) or { panic(err) }
	assert c_source.contains('__asm__ ('), c_source
	assert c_source.contains('[b] "+r" (b)'), c_source
	assert c_source.contains('[a] "r" (a)'), c_source
	$if arm64 {
		assert c_source.contains('"mov x0, %[a]\\n\\t"'), c_source
		assert c_source.contains('"str w0, [%[ptr]]\\n\\t"'), c_source
	} $else $if amd64 {
		assert c_source.contains('"mov %[a], %%rax\\n\\t"'), c_source
		assert c_source.contains('"movq \\$7, (%[ptr])\\n\\t"'), c_source
	}
	compile := os.execute('${v3_bin} -cc clang -o ${bin_path} ${source_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '10\n6\n7'
}

fn test_i386_inline_asm_reaches_c_lowering() {
	v3_bin := build_v3_inline_asm()
	source_path := '${inline_asm_tmp_path('i386_program')}.v'
	c_path := '${inline_asm_tmp_path('i386_program')}.c'
	os.write_file(source_path, 'fn main() {
	asm i386 {
		mov eax, ebx
	}
}
') or { panic(err) }
	generate := os.execute('${v3_bin} -os linux -arch i386 -cc clang -o ${c_path} ${source_path}')
	assert generate.exit_code == 0, generate.output
	assert !generate.output.contains('inline assembly is not supported'), generate.output
	c_source := os.read_file(c_path) or { panic(err) }
	assert c_source.contains('"mov %ebx, %eax\\n\\t"'), c_source
}
