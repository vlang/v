module builder

import os
import v2.pref

// Concern 1: the pre-parse self-host fast relink must never fire for a request
// that gen_cleanc() treats as "generation only" — otherwise a warm-cache
// `-o foo.c cmd/v2/v2.v` links an executable into foo.c instead of writing C.
fn test_fast_relink_skips_generation_only_outputs() {
	// A `.c` output is generation-only regardless of how it would be built.
	b_c := new_builder(&pref.Preferences{ backend: .cleanc })
	assert b_c.fast_relink_output_is_generation_only('/tmp/v2.c')
	assert b_c.fast_relink_output_is_generation_only('out.c')

	// A shared library is generation-only.
	b_shared := new_builder(&pref.Preferences{ backend: .cleanc, is_shared_lib: true })
	assert b_shared.fast_relink_output_is_generation_only('/tmp/lib')

	// A normal local executable build is NOT generation-only — the fast relink
	// is allowed to proceed (subject to its cache/freshness checks).
	b_exe := new_builder(&pref.Preferences{ backend: .cleanc })
	assert b_exe.can_compile_cleanc_locally() // sanity: host target compiles locally
	assert !b_exe.fast_relink_output_is_generation_only('/tmp/v3')
}

// Concern 2: the fast relink trusts the cc/cc_flags recorded in main.stamp, so a
// pre-parse fingerprint of the flag inputs that do NOT need parsing must change
// when the compiler / prod-shared mode / env CFLAGS change, and stay stable
// otherwise. A changed fingerprint is what invalidates a would-be stale relink.
fn test_preparse_flag_fingerprint_tracks_flag_settings() {
	base := new_builder(&pref.Preferences{ backend: .cleanc })
	fp0 := base.preparse_flag_fingerprint()

	// Stable for identical settings.
	assert new_builder(&pref.Preferences{ backend: .cleanc }).preparse_flag_fingerprint() == fp0

	// A different C compiler changes it.
	b_cc := new_builder(&pref.Preferences{ backend: .cleanc, ccompiler: 'some-other-cc' })
	assert b_cc.preparse_flag_fingerprint() != fp0

	// -prod changes it (different optimization flags).
	b_prod := new_builder(&pref.Preferences{ backend: .cleanc, is_prod: true })
	assert b_prod.preparse_flag_fingerprint() != fp0

	// -shared changes it (no -flto, different link mode).
	b_shared := new_builder(&pref.Preferences{ backend: .cleanc, is_shared_lib: true })
	assert b_shared.preparse_flag_fingerprint() != fp0

	// Env CFLAGS (V2CFLAGS) change it.
	prev := os.getenv('V2CFLAGS')
	os.setenv('V2CFLAGS', '-DV2_FAST_RELINK_TEST', true)
	fp_env := new_builder(&pref.Preferences{ backend: .cleanc }).preparse_flag_fingerprint()
	if prev == '' {
		os.unsetenv('V2CFLAGS')
	} else {
		os.setenv('V2CFLAGS', prev, true)
	}
	assert fp_env != fp0
}
