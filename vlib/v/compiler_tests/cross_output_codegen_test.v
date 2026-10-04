import os
import v.cmdexec

const vexe = @VEXE

fn cross_generate(name string, source string) string {
	return cross_generate_with('-os cross', name, source)
}

fn cross_generate_with(flags string, name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_cross_output_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'm.v')
	os.write_file(src, source) or { panic(err) }
	out := os.join_path(dir, 'out.c')
	res := os.exec([vexe, ...(os.split_args(flags) or { panic(err) }), '-o', '${out}', '${src}'])
	assert res.exit_code == 0, res.output
	return os.read_file(out) or { panic(err) }
}

fn test_cross_output_resolves_top_level_declarations_for_the_host() {
	// A declaration cannot be wrapped in `#if` by the C backend, which collects
	// nested functions, types, constants and globals directly. Keeping both
	// branches emitted both definitions unguarded and the first one silently won,
	// so a top-level `$if` holding declarations stays resolved for the host.
	c_code := cross_generate('top_level', "module main\n\n\$if windows {\n\tfn platform_marker() string {\n\t\treturn 'marker_windows'\n\t}\n} \$else {\n\tfn platform_marker() string {\n\t\treturn 'marker_nix'\n\t}\n}\n\nfn main() {\n\tprintln(platform_marker())\n}\n")
	$if windows {
		assert c_code.contains('marker_windows'), 'the host branch is missing'
		assert !c_code.contains('marker_nix'), 'the branch not taken leaked into the output'
	} $else {
		assert c_code.contains('marker_nix'), 'the host branch is missing'
		assert !c_code.contains('marker_windows'), 'the branch not taken leaked into the output'
	}
}

fn test_cross_output_validates_calls_against_the_selected_target_declarations() {
	source := 'module main\n\n\$if windows {\n\tfn windows_value() int { return 7 }\n} \$else {\n\tfn posix_value() int { return 42 }\n}\n\nfn main() {\n\t\$if windows {\n\t\tprintln(windows_value())\n\t} \$else {\n\t\tprintln(posix_value())\n\t}\n}\n'
	for flags in ['-no-retry-compilation -cross -os linux',
		'-no-retry-compilation -cross -os windows -cc msvc'] {
		c_code := cross_generate_with(flags, 'selected_calls', source)
		assert c_code.contains('windows_value('), 'the preserved Windows branch is missing'
		assert c_code.contains('posix_value('), 'the preserved POSIX branch is missing'
		assert c_code.contains('#if defined(_WIN32)'), 'the preserved calls are not guarded'
	}
}

fn test_cross_output_keeps_checker_errors_for_the_selected_target() {
	dir := os.join_path(os.vtmp_dir(), 'v3_cross_diagnostics_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'm.v')
	os.write_file(src, 'fn main() {\n\t\$if windows {\n\t\tmissing_windows_value()\n\t} \$else {\n\t\tmissing_posix_value()\n\t}\n}\n')!
	for target in ['linux', 'windows'] {
		out := os.join_path(dir, '${target}.c')
		result := os.exec([vexe, '-no-retry-compilation', '-building-v', '-cross', '-os', '${target}',
			'-o', '${out}', '${src}'])
		assert result.exit_code != 0, result.output
		selected := if target == 'windows' {
			'missing_windows_value'
		} else {
			'missing_posix_value'
		}
		inactive := if target == 'windows' {
			'missing_posix_value'
		} else {
			'missing_windows_value'
		}
		assert result.output.contains('unknown function: ${selected}'), result.output
		assert !result.output.contains('unknown function: ${inactive}'), result.output
	}
}

fn test_cross_output_compiles_generic_method_values_in_preserved_branches() {
	// Generate for a different compiler so the runtime branch is inactive while
	// V checks the source, then compile and execute that branch as portable C.
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	// os.exec reports incompatible binaries as failures on Windows too.
	use_tcc := os.user_os() != 'macos' && os.is_file(bundled_tcc)
		&& os.is_executable(bundled_tcc)
		&& os.exec([bundled_tcc, '-v']).exit_code == 0
	cc := if use_tcc {
		bundled_tcc
	} else {
		system_cc := os.find_abs_path_of_executable('cc') or {
			eprintln('skipping portable C method-value runtime test: no usable native C compiler (cc not found)')
			return
		}
		probe := os.exec([system_cc, '--version'])
		if probe.exit_code != 0 {
			eprintln('skipping portable C method-value runtime test: cc --version failed (exit ${probe.exit_code})\n${probe.output}')
			return
		}
		system_cc
	}
	generation_cc := if use_tcc { 'gcc' } else { 'tcc' }
	runtime_condition := if use_tcc { 'tinyc' } else { '!tinyc' }
	source := '
struct Box[T] {
	item T
}

fn (b Box[T]) get() T {
	return b.item
}

struct Helper {}

fn (h Helper) first[T](items []T) T {
	return items[0]
}

fn read_value[T](item T) T {
	\$if ${runtime_condition} {
		b := Box[T]{item: item}
		get := b.get
		return get()
	} \$else {
		return item
	}
}

fn main() {
	\$if ${runtime_condition} {
		b := Box[int]{item: 42}
		get := b.get
		println(get())
		pointer := &Box[u32]{item: 43}
		pp := &pointer
		get_pp := pp.get
		println(get_pp())
	} \$else {
		println("wrong then branch")
	}
	\$if !(${runtime_condition}) {
		println("wrong else branch")
	} \$else {
		b := Box[string]{item: "portable"}
		get := b.get
		println(get())
		pointer := &Box[u64]{item: 44}
		pp := &pointer
		ppp := &pp
		get_ppp := ppp.get
		println(get_ppp())
		\$if ${runtime_condition} {
			h := Helper{}
			first := h.first[int]
			println(first([7, 8]))
		}
	}
	println(read_value("generic"))
}
'
	c_code := cross_generate_with('-no-retry-compilation -cross -cc ${generation_cc} -gc none',
		'method_values', source)
	dir := os.join_path(os.vtmp_dir(), 'v3_cross_method_values_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	c_path := os.join_path(dir, 'out.c')
	os.write_file(c_path, c_code)!
	exe := os.join_path(dir, 'out' + $if windows { '.exe' } $else { '' })
	mut args := ['-w', '-o', exe, c_path]
	$if windows {
		args << ['-municode', '-lws2_32', '-lole32', '-ldbghelp']
	} $else {
		if use_tcc {
			tcc_lib := os.join_path(os.dir(bundled_tcc), 'lib')
			tcc_nested := os.join_path_single(tcc_lib, 'tcc')
			tcc_base := if os.is_dir(tcc_nested) { tcc_nested } else { tcc_lib }
			args << ['-B${tcc_base}', '-I${os.join_path_single(tcc_base, 'include')}', '-L${tcc_lib}']
		}
		args << ['-lm', '-lpthread']
	}
	compiled := cmdexec.run(cc, args)
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.replace('\r\n', '\n').trim_space() == '42\n43\nportable\n44\n7\ngeneric', run.output
}

fn test_cross_output_suppresses_inactive_deferred_warnings() {
	source := '
fn fallible() !int {
	return 42
}

fn main() {
	\$if tinyc {
		fallible()
	} \$else {
		println(fallible() or { 0 })
	}
}
'
	c_code := cross_generate_with('-no-retry-compilation -W -cross -cc gcc -gc none',
		'inactive_diagnostics', source)
	assert c_code.contains('fallible(')
	dir := os.join_path(os.vtmp_dir(), 'v3_cross_active_warning_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'm.v')
	os.write_file(src, source)!
	active := cmdexec.run(vexe, ['-no-retry-compilation', '-W', '-cross', '-cc', 'tcc', '-gc',
		'none', '-o', os.join_path(dir, 'out.c'), src])
	assert active.exit_code != 0, active.output
	assert active.output.contains('fallible() returns `!int`'), active.output
}

fn test_cross_output_allows_unavailable_types_in_preserved_branches() {
	source := 'fn main() {\n\t\$if tinyc {\n\t\tvalue := MissingTargetType{}\n\t\tprintln(value)\n\t} \$else {\n\t\tprintln("ok")\n\t}\n}\n'
	c_code := cross_generate_with('-no-retry-compilation -W -cross -cc gcc -gc none',
		'inactive_types', source)
	assert c_code.contains('MissingTargetType')
}

fn test_cross_output_keeps_directives_of_every_branch_behind_guards() {
	// Directives are the one thing the backend *can* guard, so both branches have
	// to survive: the snapshot is compiled on a machine the generator never saw.
	c_code := cross_generate('directives', "module main\n\n\$if windows {\n\t#include <winsock2.h>\n} \$else {\n\t#include <sys/select.h>\n}\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert c_code.contains('#include <winsock2.h>'), c_code.all_before('typedef')
	assert c_code.contains('#include <sys/select.h>'), c_code.all_before('typedef')
	assert c_code.contains('#if defined(_WIN32)'), 'the windows include is not guarded'
	assert c_code.contains('#if !(defined(_WIN32))'), 'the non-windows include is not guarded'
}

fn test_cross_output_refuses_a_different_pointer_width() {
	// `int`, the type layouts and the literal ranges are baked while generating,
	// so a consumer of another width has to fail loudly instead of disagreeing
	// with the `$if x64`/`$if x32` branches the preprocessor picks.
	c_code := cross_generate('width', "module main\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert c_code.contains('#error "this C was generated by `v -os cross`')
	$if x64 {
		assert c_code.contains('#if !defined(TARGET_IS_64BIT)')
	} $else {
		assert c_code.contains('#if !defined(TARGET_IS_32BIT)')
	}
}

fn test_cross_reports_its_custom_defines_in_the_generated_header() {
	// `gen_vc_ci.yml` greps a freshly generated snapshot for exactly these lines
	// to confirm it was built with `-cross`.
	c_code := cross_generate_with('-cross', 'defines', "module main\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert c_code.contains('Turned ON custom defines: no_backtrace,cross'), c_code.all_before('typedef')
	assert c_code.contains('#define CUSTOM_DEFINE_cross')
	assert c_code.contains('#define CUSTOM_DEFINE_no_backtrace')
}

fn test_cross_is_a_modifier_that_keeps_an_explicit_target() {
	// `v -cross -os windows -cc msvc` builds the Windows snapshot: `-cross` must
	// not be swallowed by, or swallow, the `-os` that follows it.
	c_code := cross_generate_with('-cross -os windows -cc msvc', 'windows', "module main\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert c_code.contains('Turned ON custom defines: no_backtrace,cross'), 'cross mode was lost'
	assert c_code.contains('#define CUSTOM_DEFINE_cross')
	// The target still selected Windows.
	assert c_code.contains('_WIN32'), 'the explicit -os windows target was lost'
}

fn test_cross_windows_output_orders_windows_header_before_bcrypt() {
	c_code := cross_generate_with('-cross -os windows -cc msvc', 'windows_bcrypt', 'module main\n\nimport crypto.rand\n\nfn main() {\n\tmut buffer := []u8{len: 1}\n\tcrypto.rand.read(mut buffer) or {}\n}\n')
	windows_index := c_code.index('#include <windows.h>') or {
		assert false, 'the Windows base header is missing'
		return
	}
	bcrypt_index := c_code.index('#include <bcrypt.h>') or {
		assert false, 'the BCrypt header is missing'
		return
	}
	assert windows_index < bcrypt_index, c_code.all_before('typedef signed char i8;')
}

fn test_cross_output_leaves_the_atomic_helpers_to_the_windows_tcc_header() {
	// The generated C may run through TCC even if it was generated for another
	// compiler. V's WinAPI atomic header is
	// emitted behind `_WIN32 && (__TINYC__ || MSVC)` and defines `atomic_fetch_add_byte` and
	// friends as function-like macros, so the backend's own `static inline`
	// definitions have to sit behind the negation of that same guard. Without it
	// the macro expanded over the definition and tcc rejected `vc/v_win.c` with
	// `redefinition of 'ManualInterlockedExchangeAdd8'`.
	for flags in ['-cross -os windows -cc msvc', '-os cross', '-os windows -cc gcc'] {
		c_code := cross_generate_with(flags, 'atomics', "module main\n\nfn main() {\n\tprintln('ok')\n}\n")
		guard := '#if !(defined(_WIN32) && (defined(__TINYC__) || (defined(_MSC_VER) && !defined(__clang__))))'
		definition := 'static inline byte atomic_fetch_add_byte('
		at := c_code.index(definition) or {
			assert false, '${flags}: the atomic helpers are missing from the snapshot'
			return
		}
		opened := c_code[..at].clone().last_index(guard) or {
			assert false, '${flags}: the atomic helpers are not guarded against the Windows TCC header'
			return
		}
		// The guard has to still be open where the helper is defined.
		between := c_code[opened..at].clone()
		assert between.count('#endif') < between.count('#if'), '${flags}: the guard closed before the atomic helpers'
	}
}

fn test_windows_msvc_output_balances_atomic_header_guards() {
	c_code := cross_generate_with('-os windows -cc msvc', 'windows_msvc_atomics', "module main\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert c_code.contains('static inline byte atomic_fetch_add_byte(')
	mut depth := 0
	for line in c_code.split_into_lines() {
		directive := line.trim_space()
		if directive.starts_with('#if') {
			depth++
		} else if directive.starts_with('#endif') {
			depth--
			assert depth >= 0, 'unmatched #endif in generated MSVC C'
		}
	}
	assert depth == 0, '${depth} unterminated #if directives in generated MSVC C'
}

fn test_cross_output_keeps_the_posix_semaphore_off_apple() {
	// A snapshot generated on Linux is compiled on macOS to bootstrap v1, and the
	// `sync` file it bakes in is the POSIX one. Apple has no `sem_timedwait` symbol
	// at all, and the rest of its unnamed POSIX semaphore API is a stub: `sem_init`
	// fails with ENOSYS and every later call on that `sem_t` fails with EBADF. So
	// each of these calls has to stay behind a guard that is false on Apple, or the
	// snapshot either fails to compile there, or panics with `Bad file descriptor`
	// on the first semaphore it waits on.
	c_code := cross_generate_with('-cross -os linux', 'semaphore', 'module main\n\nimport sync\n\nfn main() {\n\tmut sem := sync.new_semaphore()\n\tsem.post()\n\tsem.wait()\n\tprintln(sem.try_wait())\n\tprintln(sem.timed_wait(1))\n\tsem.destroy()\n}\n')
	for call in ['sem_init(', 'sem_post(', 'sem_wait(', 'sem_trywait(', 'sem_timedwait(', 'sem_destroy('] {
		assert c_code.contains(call), '`${call}` is missing from the snapshot'
		mut searched := c_code
		for {
			at := searched.index(call) or { break }
			before := searched[..at].clone()
			opened := before.last_index('#if ') or {
				assert false, 'a ${call} call is not behind any preprocessor guard'
				return
			}
			condition := before[opened..].all_before('\n')
			assert condition.contains('__APPLE__'), 'a ${call} call is guarded by `${condition}`, which is also true on Apple'
			searched = searched[at + call.len..].clone()
		}
	}
}

fn test_cross_output_uses_getentropy_instead_of_the_linux_syscall_where_unavailable() {
	// The portable snapshot is generated on Linux, so it bakes in rand_linux.c.v.
	// It is then compiled on macOS or OpenBSD to bootstrap v1, where SYS_getrandom
	// does not exist. Keep both implementations in the snapshot and let the target
	// C preprocessor select getentropy on those hosts.
	c_code := cross_generate_with('-cross -os linux', 'crypto_rand', 'module main\n\nimport crypto.rand\n\nfn main() {\n\tassert rand.bytes(1)!.len == 1\n}\n')
	body := function_body(c_code, 'i64 internal__getrandom(i64 bytes_needed, void* buffer) {')
	getentropy_at := body.index('getentropy(') or {
		assert false, 'the Apple entropy implementation is missing from the snapshot: ${body}'
		return
	}
	syscall_at := body.index('syscall(') or {
		assert false, 'the Linux entropy implementation is missing from the snapshot: ${body}'
		return
	}
	assert getentropy_at < syscall_at, 'the Apple entropy branch should precede the Linux fallback: ${body}'
	before_getentropy := body[..getentropy_at]
	guard_at := before_getentropy.last_index('#if ') or {
		assert false, 'getentropy is not behind an Apple preprocessor guard: ${body}'
		return
	}
	condition := before_getentropy[guard_at..].all_before('\n')
	assert condition.contains('__APPLE__'), 'getentropy is guarded by `${condition}`, which does not select Apple'
	assert condition.contains('__OpenBSD__'), 'getentropy is guarded by `${condition}`, which does not select OpenBSD'
	assert body[getentropy_at..syscall_at].contains('#else'), 'the Linux syscall is not in the fallback branch: ${body}'
}

fn test_cross_output_keeps_the_linux_calls_of_the_diagnostics_server_behind_linux_guards() {
	// The snapshot bakes in diagserver_linux.c.v too, and is compiled on macOS,
	// which has neither memfd_create nor prctl. Each of these calls has to stay
	// behind a guard that holds on Linux alone, or the snapshot does not compile
	// there.
	c_code := cross_generate_with('-cross -os linux', 'diagserver', 'module main\n\nimport v.diagserver\n\nfn main() {\n\tmut request := diagserver.serve()\n\tif request.diagnose_in_grandchild() {\n\t\treturn\n\t}\n}\n')
	for call in ['SYS_memfd_create', 'prctl('] {
		assert c_code.contains(call), '`${call}` is missing from the snapshot'
		mut from := 0
		for {
			at := c_code.index_after(call, from) or { break }
			guards := enclosing_guards_at(c_code, at)
			assert guards.any(!it.starts_with('!') && it.contains('defined(__linux__)')
				&& !it.contains('!defined(__linux__)')), 'a `${call}` call is guarded by ${guards}, which also hold outside Linux'
			from = at + call.len
		}
	}
}

fn test_cross_output_keeps_a_working_clock_on_apple() {
	// `time` splits per platform too, so a snapshot generated on Linux bakes the
	// stand-ins from time_linux.c.v, while the preprocessor still takes the
	// `__APPLE__` branch of the shared code when it is compiled on macOS. Those
	// stand-ins used to return zero, which left the bootstrapped compiler with a
	// clock that never advanced: `v` divided by its own elapsed parse time and
	// died with `division by zero`. They have to answer with real POSIX time.
	c_code := cross_generate_with('-cross -os linux', 'clock', 'module main\n\nimport time\n\nfn main() {\n\tprintln(time.sys_mono_now())\n\tprintln(time.now())\n\tprintln(time.utc())\n}\n')
	mono := function_body(c_code, 'u64 time__sys_mono_now_darwin(void) {')
	assert mono.contains('clock_gettime'), 'the snapshot cannot read a monotonic clock on Apple: ${mono}'
	now := function_body(c_code, 'time__Time time__darwin_now(void) {')
	assert now.contains('time__linux_now()'), 'the snapshot cannot read the local time on Apple: ${now}'
	utc := function_body(c_code, 'time__Time time__darwin_utc(void) {')
	assert utc.contains('time__linux_utc()'), 'the snapshot cannot read UTC on Apple: ${utc}'
}

fn test_cross_output_lets_the_target_libc_pick_the_poll_header() {
	// The portable snapshot is generated on glibc Linux and later compiled on musl
	// too. musl warns about <sys/poll.h>, which fails consumers building with
	// `-Werror`, while the linuxroot sysroot ships only <sys/poll.h>. A `$if musl ?`
	// check is folded for the generating host, so the target C preprocessor has to
	// pick the header from the libc it actually compiles against.
	c_code := cross_generate_with('-cross -os linux', 'cmdexec_poll', "module main\n\nimport v.cmdexec\n\nfn main() {\n\tprintln(cmdexec.run('true', []string{}).exit_code)\n}\n")
	sys_poll_at := c_code.index('#include <sys/poll.h>') or {
		assert false, 'the glibc poll header is missing from the snapshot'
		return
	}
	before := c_code[..sys_poll_at]
	guard_at := before.last_index('#if ') or {
		assert false, '<sys/poll.h> is not behind any preprocessor guard'
		return
	}
	condition := before[guard_at..].all_before('\n')
	assert condition.contains('__GLIBC__'), '<sys/poll.h> is guarded by `${condition}`, which does not check for glibc'
	fallback := c_code[sys_poll_at..].all_before('#endif')
	assert fallback.contains('#else\n#include <poll.h>'), 'musl and the other targets lost <poll.h>: ${fallback}'
}

fn test_glibc_hello_world_declares_the_array_constructors_it_calls() {
	// A literal-output program skips markused's runtime seeds, while the glibc
	// backtrace it reaches through `panic` passes an argument array to `addr2line`.
	c_code := cross_generate_with('-os linux -glibc', 'glibc_hello', "fn main() {\n\tprintln('Hello World!')\n}\n")
	for ctor in ['new_array_from_c_array', 'new_array_from_c_array_noscan'] {
		if c_code.contains('(${ctor}(') {
			assert c_code.contains('\narray ${ctor}('), '`${ctor}` is called but never declared'
		}
	}
}

fn test_cross_windows_output_guards_the_msvc_only_headers() {
	// `vc/v_win.c` is generated with `-cross -os windows -cc msvc` and then built
	// by makev.bat with the bundled TinyCC, which ships neither <intrin.h> nor
	// <dbghelp.h> on Windows. Deciding that at generation time baked both into
	// every Windows snapshot, so the bootstrap died on
	// `include file 'intrin.h' not found` before compiling a line of V. The
	// snapshot is compiled by a different C compiler than the one it was generated
	// for, so the choice belongs to the C preprocessor. See #29146.
	c_code := cross_generate_with('-cross -os windows -cc msvc', 'msvc_headers',
		"module main\n\nfn main() {\n\tprintln('ok')\n}\n")
	guard_error := msvc_only_header_guard_error(c_code)
	assert guard_error == '', guard_error
}

// msvc_only_header_guard_error checks every include, accepting only explicit positive
// MSVC directives in its active branches. Other condition spellings fail conservatively.
fn msvc_only_header_guard_error(c_code string) string {
	for header in ['#include <intrin.h>', '#include <dbghelp.h>'] {
		mut start := 0
		mut found := false
		for {
			at := c_code.index_after(header, start) or { break }
			found = true
			guards := enclosing_guards_at(c_code, at)
			if !guards.any(it in ['#if defined(_MSC_VER)', '#ifdef _MSC_VER', '#elif defined(_MSC_VER)']) {
				return '${header} at byte ${at} is not behind a positive MSVC-only guard: ${guards}'
			}
			start = at + header.len
		}
		if !found {
			return '${header} is missing from the Windows snapshot: the MSVC intrinsics and the dbghelp backtraces need it'
		}
	}
	return ''
}

fn test_msvc_only_header_guard_assertion_rejects_unguarded_and_negative_branches() {
	headers := '#include <intrin.h>\n#include <dbghelp.h>\n'
	guarded := '#if defined(_MSC_VER)\n${headers}#endif\n'
	for c_code in [
		'${guarded}#include <intrin.h>\n',
		'${guarded}#include <dbghelp.h>\n',
		'#ifdef _WIN32\n${guarded}${headers}#endif\n',
		'#ifndef _MSC_VER\n${headers}#endif\n',
		'#if !defined(_MSC_VER)\n${headers}#endif\n',
		'#if defined(_MSC_VER)\n#else\n${headers}#endif\n',
		'#if defined(_MSC_VER)\n#elif 1\n${headers}#endif\n',
		'#if 0\n#elif defined(_MSC_VER)\n#else\n${headers}#endif\n',
		'#if defined(_MSC_VER) || defined(__TINYC__)\n${headers}#endif\n',
		'#if defined(_MSC_VER)\n#endif\n${headers}',
		'#if defined(_MSC_VER)\n#include <intrin.h>\n#endif\n',
		'#if defined(_MSC_VER)\n#include <dbghelp.h>\n#endif\n',
	] {
		assert msvc_only_header_guard_error(c_code) != '', 'accepted unsafe or incomplete includes:\n${c_code}'
	}
}

fn test_msvc_only_header_guard_assertion_accepts_active_positive_branches() {
	headers := '#include <intrin.h>\n#include <dbghelp.h>\n'
	for c_code in [
		'#if defined(_MSC_VER)\n${headers}${headers}#endif\n',
		'#ifdef _WIN32\n#ifdef _MSC_VER\n${headers}#endif\n#endif\n',
		'#if 0\n#elif defined(_MSC_VER)\n${headers}#endif\n',
		'#if defined(_MSC_VER)\n#if 0\n#else\n${headers}#endif\n#endif\n',
	] {
		guard_error := msvc_only_header_guard_error(c_code)
		assert guard_error == '', '${guard_error}\n${c_code}'
	}
}

fn test_cross_windows_output_includes_the_same_headers_for_every_c_compiler() {
	// A snapshot's header set is a promise about the C compiler that will compile
	// it, and that compiler is picked at build time, not generation time: the same
	// `vc/v_win.c` gets built by tcc, clang and gcc (makev.bat), and the bundled
	// tcc is the one CI uses first. So `-cc` must not change which headers the
	// snapshot includes - only the C preprocessor may. See #29146, where
	// `if g.ccompiler == 'msvc'` made the msvc spelling the only buildable one.
	mut reference := []string{}
	for ccompiler in ['msvc', 'gcc', 'clang', 'tcc'] {
		c_code := cross_generate_with('-cross -os windows -cc ${ccompiler}', 'headers_${ccompiler}',
			'module main\n\nimport crypto.rand\n\nfn main() {\n\tmut buffer := []u8{len: 1}\n\tcrypto.rand.read(mut buffer) or {}\n}\n')
		guard_error := msvc_only_header_guard_error(c_code)
		assert guard_error == '', '-cc ${ccompiler}: ${guard_error}'
		includes := c_code.split_into_lines().filter(it.trim_space().starts_with('#include'))
			.map(it.trim_space())
		if reference.len == 0 {
			reference = includes.clone()
			continue
		}
		only_here := includes.filter(it !in reference)
		only_there := reference.filter(it !in includes)
		assert only_here.len == 0 && only_there.len == 0, '-cc ${ccompiler} emits ${only_here} but the msvc spelling emits ${only_there} instead'
	}
}

// enclosing_guards_at returns the active branch directives at `at`, outermost first.
// An `#elif` replaces the preceding branch and an `#else` negates it. Earlier sibling
// conditions are omitted: only an explicit positive directive proves MSVC-only use.
fn enclosing_guards_at(c_code string, at int) []string {
	mut stack := []string{}
	mut in_else := []bool{}
	for line in c_code[..at].split_into_lines() {
		directive := line.trim_space()
		if directive.starts_with('#if') {
			stack << directive.all_before('\n')
			in_else << false
		} else if directive.starts_with('#elif') && stack.len > 0 {
			stack[stack.len - 1] = directive
			in_else[in_else.len - 1] = false
		} else if directive.starts_with('#else') && in_else.len > 0 {
			in_else[in_else.len - 1] = true
		} else if directive.starts_with('#endif') && stack.len > 0 {
			stack.delete_last()
			in_else.delete_last()
		}
	}
	mut guards := []string{}
	for i, condition in stack {
		guards << if in_else[i] { '!(${condition})' } else { condition }
	}
	return guards
}

fn test_top_level_asm_is_emitted_with_its_reference() {
	c_code := cross_generate_with('-os linux -arch amd64 -gc none', 'top_level_asm', 'module main\n\nfn main() {\n\tmut value := int(0)\n\tasm amd64 {\n\t\tmov value, [rip + word_sequence]\n\t\t; =r (value)\n\t}\n\tassert value == 0x480f3527\n}\n\nasm amd64 {\n\t.global word_sequence\n\tword_sequence:\n\t.long 0x480f3527\n}\n')
	assert c_code.contains('".global word_sequence\\n\\t"'), 'the top-level asm block is missing'
	assert c_code.contains('"word_sequence:\\n\\t"'), 'the top-level asm label is missing'
	assert c_code.contains('"mov word_sequence(%%rip), %[value]\\n\\t"'), 'the reference to the asm label is missing'
}

// function_body returns the source of the C function that `signature` opens.
fn function_body(c_code string, signature string) string {
	at := c_code.index(signature) or {
		assert false, '`${signature}` is missing from the snapshot'
		return ''
	}
	rest := c_code[at + signature.len..]
	return rest.all_before('\n}')
}
