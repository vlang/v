import os
import time
import v.cmdexec

fn test_cached_builtin_survives_new_error_implementers_and_module_misses() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_cache_builtin_reuse_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// Keep the compiler configuration and imported modules fixed while changing
	// the program's error implementations. Each file is a fresh test program.
	for index in 0 .. 3 {
		name := 'PayloadError${index}'
		source := os.join_path(root, 'program_${index}_test.v')
		os.write_file(source, '
import os
import strconv
struct ${name} {
	Error
	value int
}
fn (e ${name}) msg() string { return "payload " + e.value.str() }
fn (e ${name}) code() int { return e.value }
fn test_error_dispatch() {
	err := IError(${name}{value: 42})
	assert err.msg() == "payload 42"
	assert err.code() == 42
	assert err.str().contains("payload 42")
	assert strconv.atoi("42")! == 42
	assert os.getpid() > 0
}
')!
		result := cmdexec.run_in('env', ['-u', 'VEXE', '-u', 'VFLAGS', 'V3_CACHE_TRACE=1',
			'V3CACHE=${root}/cache', @VEXE, '-new-compiler', '-v', source], root)
		assert result.exit_code == 0, result.output
		if index == 0 {
			assert result.output.contains('Caching module builtin...'), result.output
		} else {
			assert !result.output.contains('Caching module builtin...'), result.output
			assert result.output.contains('object miss: module=os'), result.output
			// An imported object's interface signature can miss; that retry must
			// still parse the builtin declaration header rather than its sources.
			assert result.output.contains('.vh files:'), result.output
			assert !result.output.contains('/builtin/array.v'), result.output
		}
	}
}
