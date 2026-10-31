// vtest build: false

module fastc

import os
import strings
import time

fn fastc_stdin_test_slurp(fd int) string {
	defer { os.fd_close(fd) }
	return os.fd_slurp(fd).join('')
}

fn fastc_stdin_test_dir() string {
	root := os.join_path(os.vtmp_dir(), 'fastc_stdin_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn fastc_stdin_test_pieces() ([]string, FastcUnitLayout) {
	pieces := ['ignored definition', 'ignored prototypes', 'solo\x00\n',
		'a'.repeat(fastc_unit_stdin_buffer_size - 1), '', '\na-end\n',
		'b'.repeat(fastc_unit_stdin_buffer_size), '\x00',
		'c'.repeat(fastc_unit_stdin_buffer_size + 17), 'finish\n']
	units := FastcUnitLayout{
		head_end:           2
		solo_end:           3
		prototype_start:    1
		prototype_end:      2
		unit_starts:        [3, 6, 10]
		extern_indexes:     [0]
		extern_texts:       ['extern int shared;\n']
		define_texts:       ['int shared;\n']
		prototype_texts:    ['void first(void);\n', 'void second(void);\n', 'void tail(void);\n',
			'void unused(void);\n']
		unit_ref_starts:    [0, 1, 2]
		unit_ref_ids:       [0, 1]
		solo_prototype_ids: [1]
		tail_prototype_ids: [2]
	}
	return pieces, units
}

fn test_fastc_stdin_coalescing_preserves_bytes_and_buffer_bound() {
	mut pipe := os.pipe() or { panic(err) }
	defer { pipe.close() }
	reader := spawn fastc_stdin_test_slurp(pipe.read_fd)
	pipe.read_fd = -1
	mut out := strings.new_builder(fastc_unit_stdin_buffer_size)
	defer { unsafe { out.free() } }
	pieces := ['', 'a'.repeat(fastc_unit_stdin_buffer_size - 1), '\x00', 'small',
		'b'.repeat(fastc_unit_stdin_buffer_size), 'suffix',
		'c'.repeat(fastc_unit_stdin_buffer_size + 17), '',
		'd'.repeat(fastc_unit_stdin_buffer_size - 1), 'ef', 'finish']
	for piece in pieces {
		assert fastc_append_c_unit_piece(pipe.write_fd, mut out, piece)
		assert out.len <= fastc_unit_stdin_buffer_size
		assert out.cap == fastc_unit_stdin_buffer_size
	}
	assert fastc_flush_c_unit_stdin(pipe.write_fd, mut out)
	assert out.len == 0
	os.fd_close(pipe.write_fd)
	pipe.write_fd = -1
	assert reader.wait() == pieces.join('')
}

fn test_fastc_stdin_render_matches_complete_translation_units() {
	pieces, units := fastc_stdin_test_pieces()
	groups := [0, 1, 0]
	firsts := [0, 1, 0]
	ends := [1, 2, 2]
	for i, g in groups {
		pipe := os.pipe() or { panic(err) }
		reader := spawn fastc_stdin_test_slurp(pipe.read_fd)
		assert fastc_render_c_unit_stdin(pipe.write_fd, pieces, &units, g, firsts[i], ends[i])
		assert reader.wait() == fastc_render_c_unit(pieces, &units, g, firsts[i], ends[i])
	}
}

fn test_fastc_stdin_string_writer_preserves_bytes_and_closes_at_eof() {
	for source in ['', 'prefix\x00' + 'x'.repeat(2 * fastc_unit_stdin_buffer_size) + '\nend'] {
		pipe := os.pipe() or { panic(err) }
		reader := spawn fastc_stdin_test_slurp(pipe.read_fd)
		assert fastc_write_unit_stdin(pipe.write_fd, source)
		assert reader.wait() == source
	}
	assert !fastc_write_unit_stdin(-1, 'not writable')
	pieces, units := fastc_stdin_test_pieces()
	assert !fastc_render_c_unit_stdin(-1, pieces, &units, 0, 0, 2)
}

fn fastc_stdin_test_prepared(units &FastcPrestartedCUnits) FastcPreparedUnits {
	return FastcPreparedUnits{
		entries: units.objects.map(FastcUnitCacheEntry{ object: it })
		objects: units.objects.clone()
	}
}

fn fastc_stdin_test_assert_reaped(pids []int) {
	for pid in pids {
		mut status := 0
		C.errno = 0
		waited := C.waitpid(pid, &status, C.WNOHANG)
		wait_errno := C.errno
		assert waited == -1
		assert wait_errno == C.ECHILD
	}
}

fn test_fastc_stdin_prestarted_units_observe_eof_and_keep_original_order() {
	$if !macos {
		return
	}
	root := fastc_stdin_test_dir()
	defer { os.rmdir_all(root) or {} }
	compiler := os.join_path(root, 'capture-compiler')
	os.write_file(compiler, '#!/bin/sh\nobject=\nwhile [ "$#" -gt 0 ]; do\nif [ "$1" = -o ]; then shift; object="$1"; fi\nshift\ndone\nexec /bin/cat > "$object"\n') or {
		panic(err)
	}
	os.chmod(compiler, 0o755) or { panic(err) }
	work := os.join_path(root, 'build')
	mut prestarted := fastc_prestart_c_units(compiler, [], work, 2)
	assert prestarted.active
	defer { fastc_discard_prestarted_c_units(mut prestarted) }
	pids := prestarted.pids.clone()
	pieces, units := fastc_stdin_test_pieces()
	prefix := os.join_path(work, 'src')
	plan := fastc_c_unit_plan(prefix, pieces, units, 2)
	prepared := fastc_stdin_test_prepared(&prestarted)
	mut feeding := fastc_begin_render_prestarted_c_units(mut prestarted, prefix, pieces, units,
		2) or { panic(err) }
	mut prepared_link := FastcPreparedLink{}
	objects := fastc_finish_prestarted_c_units(mut prestarted, mut feeding, prepared, mut
		prepared_link, false) or { panic(err) }
	assert objects == prepared.objects
	assert !prestarted.active
	assert prestarted.pids.all(it == 0)
	assert prestarted.read_fds.all(it == -1)
	assert prestarted.write_fds.all(it == -1)
	fastc_stdin_test_assert_reaped(pids)
	for i, object in objects {
		assert os.read_file(object) or { panic(err) } == fastc_render_c_unit(pieces, &units,
			i, plan.first_units[i], plan.first_units[i + 1])
	}
}

fn fastc_stdin_test_closed_input(exit_code int) {
	root := fastc_stdin_test_dir()
	defer { os.rmdir_all(root) or {} }
	compiler := os.join_path(root, 'closed-input-compiler')
	diagnostics := if exit_code == 0 {
		''
	} else {
		'i=0\nwhile [ "$i" -lt 8192 ]; do printf "early input failure\\n" >&2; i=$((i + 1)); done\n'
	}
	os.write_file(compiler, '#!/bin/sh\nexec 0<&-\n' + diagnostics + 'exit ${exit_code}\n') or {
		panic(err)
	}
	os.chmod(compiler, 0o755) or { panic(err) }
	work := os.join_path(root, 'build')
	mut prestarted := fastc_prestart_c_units(compiler, [], work, 2)
	assert prestarted.active
	defer { fastc_discard_prestarted_c_units(mut prestarted) }
	pids := prestarted.pids.clone()
	pieces, units := fastc_stdin_test_pieces()
	prepared := fastc_stdin_test_prepared(&prestarted)
	mut feeding := fastc_begin_render_prestarted_c_units(mut prestarted, os.join_path(work,
		'src'), pieces, units, 2) or { panic(err) }
	mut prepared_link := FastcPreparedLink{}
	fastc_finish_prestarted_c_units(mut prestarted, mut feeding, prepared, mut prepared_link,
		false) or {
		if exit_code == 0 {
			assert err.msg().contains('could not write FastC source')
		} else {
			assert err.msg().contains('early input failure')
		}
		assert !prestarted.active
		assert prestarted.pids.all(it == 0)
		assert prestarted.read_fds.all(it == -1)
		assert prestarted.write_fds.all(it == -1)
		fastc_stdin_test_assert_reaped(pids)
		return
	}
	assert false, 'expected closed stdin to fail even when the compiler exits zero'
}

fn test_fastc_stdin_prestarted_units_reap_all_children_on_write_or_compile_failure() {
	$if !macos {
		return
	}
	fastc_stdin_test_closed_input(0)
	fastc_stdin_test_closed_input(7)
}
