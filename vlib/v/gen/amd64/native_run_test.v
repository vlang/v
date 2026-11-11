module amd64

import os

const amd64_test_compiler = os.join_path(os.vtmp_dir(), 'amd64_run_compiler_${os.getpid()}')

fn testsuite_end() {
	os.rm(amd64_test_compiler) or {}
}

fn amd64_test_compiler_path() string {
	if !os.exists(amd64_test_compiler) {
		result := os.exec([@VEXE, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend', 'amd64',
			'-o', amd64_test_compiler, os.join_path(@VEXEROOT, 'cmd', 'v')])
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
		compiled := os.exec([compiler, '-gc', 'none', '-b', 'amd64', '-o', output, source])
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

		built := os.exec([compiler, '-gc', 'none', '-b', 'amd64', '-o', executable, source])
		assert built.exit_code == 0, built.output
		bytes := os.read_bytes(executable)!
		assert bytes.len >= 64
		assert bytes[..4] == [u8(0x7f), 0x45, 0x4c, 0x46]
		assert bytes[4] == 2 // ELFCLASS64
		assert bytes[5] == 1 // little endian
		assert bytes[16] == 2 // ET_EXEC
		assert bytes[18] == 62 // EM_X86_64
		assert bytes[19] == 0
		result := os.exec([executable])
		assert result.exit_code == 0, result.output
		assert result.output == 'Hello, world!\n', result.output

		built_object := os.exec([compiler, '-gc', 'none', '-b', 'amd64', '-o', object, source])
		assert built_object.exit_code == 0, built_object.output
		object_bytes := os.read_bytes(object)!
		assert object_bytes.len >= 64
		assert object_bytes[..4] == [u8(0x7f), 0x45, 0x4c, 0x46]
		assert object_bytes[16] == 1 // ET_REL
		assert object_bytes[18] == 62 // EM_X86_64

		compat := os.exec([compiler, '-gc', 'none', '-b', 'x64', '-o', executable, source])
		assert compat.exit_code == 0, compat.output
		compat_run := os.exec([executable])
		assert compat_run.exit_code == 0, compat_run.output
		assert compat_run.output == 'Hello, world!\n', compat_run.output
	}
}
