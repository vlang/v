module pref

import os

// A wasm build must compile `foo.wasm.v` and not the c-family `foo.c.v` that
// the same directory also holds. The wasm file family is decided here, so the
// predicate is checked on its own as well as through the directory selection.
fn test_wasm_source_file_matches_backend_takes_the_wasm_family() {
	for accepted in ['foo.v', '/tmp/mod/foo.v', 'foo.wasm.v', 'foo_nix.v', 'foo_default.v',
		'foo_wasm32_emscripten.wasm.v'] {
		assert wasm_source_file_matches_backend(accepted), accepted
	}
	for rejected in ['foo.c.v', 'foo.js.v', 'foo.native.v', 'foo.arm64.v', 'foo.amd64.v', 'foo.rv64.v'] {
		assert !wasm_source_file_matches_backend(rejected), rejected
	}
}

// backend_family_files is a directory holding one file per backend family, plus
// the platform and define qualified variants a real module directory mixes in.
const backend_family_files = ['plain.v', 'feature.wasm.v', 'feature.c.v', 'feature.js.v',
	'feature.native.v', 'feature.arm64.v', 'feature_amd64.v', 'guest_windows.v',
	'guest_wasm32_emscripten.v', 'helper_nix.v', 'helper_default.c.v', 'promo_d_wasm32_emscripten.c.v',
	'promo_notd_wasm32_emscripten.c.v', 'member_test.v']

fn backend_fixture_dir(tag string, names []string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_pref_backend_${tag}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	for name in names {
		os.write_file(os.join_path(dir, name), 'module sample\n') or { panic(err) }
	}
	return dir
}

// The wasm target used to pull in every backend's file, because the only
// backend filter in the directory selection was the one that skips `.js.v`.
fn test_wasm_backend_selects_wasm_file_over_other_backends_files() {
	dir := backend_fixture_dir('family', backend_family_files)
	defer {
		os.rmdir_all(dir) or {}
	}
	wasm := target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	selected := get_v_files_from_dir_for_backend_target(dir, [], 'wasm', wasm).map(os.base(it))
	assert selected == ['feature.wasm.v', 'guest_wasm32_emscripten.v', 'helper_nix.v', 'plain.v']
	// A define never brings a c-family file back into a wasm build.
	selected_with_define := get_v_files_from_dir_for_backend_target(dir, ['wasm32_emscripten'], 'wasm',
		wasm).map(os.base(it))
	assert selected_with_define == selected
}

// The wasm family is not only `.wasm.v`: the `_nix.` and `_default.` styled
// fallbacks the wasm target accepts anyway keep being accepted, and the
// c-family fallback is still dropped.
fn test_wasm_backend_keeps_the_platform_fallbacks_the_target_accepts() {
	dir := backend_fixture_dir('fallbacks', ['plain.v', 'shared_nix.v', 'shared_default.v',
		'shared_default.c.v', 'shared_wasm32_emscripten.v', 'solo_default.v', 'guest_windows.v'])
	defer {
		os.rmdir_all(dir) or {}
	}
	wasm := target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	selected := get_v_files_from_dir_for_backend_target(dir, [], 'wasm', wasm).map(os.base(it))
	assert selected == ['plain.v', 'shared_nix.v', 'shared_wasm32_emscripten.v', 'solo_default.v']
}

// `get_v_files_from_dir_for_target` is shared by every backend, so threading the
// backend name must not move the list of any backend but wasm. The pinned lists
// are the ones the backend-blind function returned before the wasm family was
// introduced, and every other backend still has to produce them exactly.
fn test_other_backends_get_the_file_list_they_had_before() {
	dir := backend_fixture_dir('shared', backend_family_files)
	defer {
		os.rmdir_all(dir) or {}
	}
	linux_amd64 := target_from('linux', 'amd64') or { panic(err) }
	linux_arm64 := target_from('linux', 'arm64') or { panic(err) }
	macos_arm64 := target_from('macos', 'arm64') or { panic(err) }
	windows_amd64 := target_from('windows', 'amd64') or { panic(err) }
	wasm := target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	for target in [linux_amd64, linux_arm64, macos_arm64, windows_amd64, wasm] {
		for backend in ['c', 'js', 'native', 'arm64', 'eval'] {
			before := get_v_files_from_dir_for_target(dir, [], target)
			after := get_v_files_from_dir_for_backend_target(dir, [], backend, target)
			assert after == before, '${backend} on ${target.os}/${target.arch}'
		}
	}
	assert get_v_files_from_dir_for_backend_target(dir, [], 'c', linux_amd64).map(os.base(it)) == [
		'feature.native.v',
		'feature.wasm.v',
		'feature_amd64.v',
		'helper_nix.v',
		'plain.v',
		'feature.c.v',
		'promo_notd_wasm32_emscripten.c.v',
	]
	assert get_v_files_from_dir_for_backend_target(dir, [], 'arm64', linux_arm64).map(os.base(it)) == [
		'feature.arm64.v',
		'feature.native.v',
		'feature.wasm.v',
		'helper_nix.v',
		'plain.v',
		'feature.c.v',
		'promo_notd_wasm32_emscripten.c.v',
	]
	assert get_v_files_from_dir_for_backend_target(dir, [], 'c', windows_amd64).map(os.base(it)) == [
		'feature.native.v',
		'feature.wasm.v',
		'feature_amd64.v',
		'guest_windows.v',
		'plain.v',
		'feature.c.v',
		'helper_default.c.v',
		'promo_notd_wasm32_emscripten.c.v',
	]
	// The backend-blind entry point keeps its old list even on a wasm target,
	// so every caller that does not ask for the wasm family is unaffected.
	assert get_v_files_from_dir_for_target(dir, [], wasm).map(os.base(it)) == [
		'feature.native.v',
		'feature.wasm.v',
		'guest_wasm32_emscripten.v',
		'helper_nix.v',
		'plain.v',
		'feature.c.v',
		'promo_notd_wasm32_emscripten.c.v',
	]
}
