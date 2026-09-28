// vtest build: windows && tinyc
// vtest vflags: -cc tcc -no-retry-compilation
import os

fn test_windows_tcc_file_reopen() {
	$if windows {
		$if !tinyc {
			$compile_error('this regression must be compiled with TinyCC')
		}
		assert @CCOMPILER == 'tinyc'
		tmp_dir := os.join_path(os.vtmp_dir(), 'windows_tcc_file_reopen_${os.getpid()}')
		os.mkdir_all(tmp_dir)!
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		original_path := os.join_path(tmp_dir, 'original.txt')
		reopened_path := os.join_path(tmp_dir, 'reopened.txt')
		os.write_file(original_path, 'before')!
		os.write_file(reopened_path, 'after')!

		mut file := os.open(original_path)!
		assert file.read_bytes(6).bytestr() == 'before'
		file.reopen(reopened_path, 'rb')!
		assert file.read_bytes(5).bytestr() == 'after'
		file.close()
	}
}

fn test_windows_tcc_command_pipe_lifecycle() {
	$if windows {
		$if !tinyc {
			$compile_error('this regression must be compiled with TinyCC')
		}
		assert @CCOMPILER == 'tinyc'
		mut command := os.start_new_command('echo windows-tcc-command-pipe')!
		mut output := ''
		for !command.eof {
			output += command.read_line()
		}
		command.close()!
		assert output.trim_space() == 'windows-tcc-command-pipe', output
		assert command.exit_code == 0
	}
}
