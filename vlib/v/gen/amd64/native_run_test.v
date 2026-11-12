module amd64

import os

const amd64_test_compiler = os.join_path(os.vtmp_dir(), 'amd64_run_compiler_${os.getpid()}')

fn testsuite_end() {
	os.rm(amd64_test_compiler) or {}
}

fn amd64_test_compiler_path() string {
	if !os.exists(amd64_test_compiler) {
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-d',
			'skip_fastc', '-compile-backend', 'amd64', '-o', amd64_test_compiler,
			os.join_path(@VEXEROOT, 'cmd', 'v')])
		assert result.exit_code == 0, result.output
	}
	return amd64_test_compiler
}

fn test_amd64_backend_compiles_and_runs_a_program() {
	$if linux && amd64 {
		compiler := amd64_test_compiler_path()
		root := os.join_path(os.vtmp_dir(), 'amd64_native_${os.getpid()}')
		os.mkdir_all(root)!
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(source, "fn main() {\n\tprintln('amd64 backend works')\n}\n")!
		compiled := os.exec([compiler, '-new-compiler', '-no-retry-compilation', '-gc', 'none',
			'-b', 'amd64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'amd64 backend works', ran.output
	}
}

// The AMD64 backend must emit a runnable Linux ELF64 executable, not just
// a relocatable object. The old x64 CLI spelling remains an alias.
fn test_amd64_hello_world_is_linux_elf() {
	$if linux && amd64 {
		compiler := amd64_test_compiler_path()
		root := os.join_path(os.vtmp_dir(), 'amd64_elf_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'hello.v')
		executable := os.join_path(root, 'hello')
		object := os.join_path(root, 'hello.o')
		os.write_file(source, "fn main() {\n\tprintln('Hello, world!')\n}\n")!

		built := os.exec([compiler, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-b',
			'amd64', '-o', executable, source])
		assert built.exit_code == 0, built.output
		bytes := os.read_bytes(executable)!
		assert bytes.len >= 64
		assert bytes[..4] == [u8(0x7f), 0x45, 0x4c, 0x46]
		assert bytes[4] == 2 // ELFCLASS64
		assert bytes[5] == 1 // little endian
		assert bytes[16..18] == [u8(2), 0] // ET_EXEC
		assert bytes[18] == 62 // EM_X86_64
		assert bytes[19] == 0
		result := os.exec([executable])
		assert result.exit_code == 0, result.output
		assert result.output == 'Hello, world!\n', result.output

		built_object := os.exec([compiler, '-new-compiler', '-no-retry-compilation', '-gc', 'none',
			'-b', 'amd64', '-o', object, source])
		assert built_object.exit_code == 0, built_object.output
		object_bytes := os.read_bytes(object)!
		assert object_bytes.len >= 64
		assert object_bytes[..4] == [u8(0x7f), 0x45, 0x4c, 0x46]
		assert object_bytes[4] == 2 // ELFCLASS64
		assert object_bytes[5] == 1 // little endian
		assert object_bytes[16..18] == [u8(1), 0] // ET_REL
		assert object_bytes[18..20] == [u8(62), 0] // EM_X86_64

		linked := os.join_path(root, 'linked_object')
		link_result := os.exec(['cc', '-no-pie', object, '-o', linked, '-lm', '-ldl', '-lpthread'])
		assert link_result.exit_code == 0, link_result.output
		object_run := os.exec([linked])
		assert object_run.exit_code == 0, object_run.output
		assert object_run.output == 'Hello, world!\n', object_run.output

		compat := os.exec([compiler, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-b',
			'x64', '-o', executable, source])
		assert compat.exit_code == 0, compat.output
		compat_run := os.exec([executable])
		assert compat_run.exit_code == 0, compat_run.output
		assert compat_run.output == 'Hello, world!\n', compat_run.output
	}
}

fn test_amd64_inline_assembly_accepts_canonical_and_legacy_backend_names() {
	$if linux && amd64 {
		compiler := amd64_test_compiler_path()
		root := os.join_path(os.vtmp_dir(), 'amd64_asm_alias_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(source, 'fn main() {
	initial := u64(1)
	mut result := u64(0)
	asm amd64 {
		addq rax, 256
		; =a (result)
		; a (initial)
		; cc
	}
	println(result)
	assert result == 257
}
')!
		for backend in ['amd64', 'x64'] {
			compiled := os.exec([compiler, '-new-compiler', '-no-retry-compilation', '-gc', 'none',
				'-b', backend, '-o', output, source])
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([output])
			assert result.exit_code == 0, result.output
			assert result.output == '257\n', result.output
		}
	}
}
