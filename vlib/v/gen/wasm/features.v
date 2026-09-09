// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

// WebAssembly feature gating for the native backend.
//
// `wasm-opt` (Binaryen) and `wasm-validate` (WABT) accept *different* flag
// spellings for the same features (e.g. Binaryen `--enable-exception-handling`
// vs WABT `--enable-exceptions`; Binaryen `--enable-nontrapping-float-to-int`
// vs WABT `--disable-saturating-float-to-int`). The logical feature set is
// therefore defined once here and rendered into each tool's own vocabulary.
//
// The default build enables only the Safari-15 "floor" features; everything
// else is opt-in via a `-d <name>` compile define. This keeps a default
// `-os browser`/`-os wasi` build from silently emitting opcodes past the
// baseline (the old `wasm-opt -all` enabled memory64/relaxed-SIMD/exnref/gc).

// wasm_opt_min_version is the Binaryen version the toolchain pins
// (see cmd/tools/install_binaryen.vsh). Used only for a soft warning.
const wasm_opt_min_version = 112

enum WasmFeature {
	reference_types
	bulk_memory
	multivalue
	sign_ext
	mutable_globals
	nontrapping_f2i
	simd
	gc
	exception_handling
	tail_call
	threads
	memory64
	relaxed_simd
	extended_const
}

// wasm_all_features lists every modelled feature, used to compute the WABT
// validator argument set relative to its defaults.
const wasm_all_features = [WasmFeature.reference_types, .bulk_memory, .multivalue, .sign_ext,
	.mutable_globals, .nontrapping_f2i, .simd, .gc, .exception_handling, .tail_call, .threads,
	.memory64, .relaxed_simd, .extended_const]

// wasm_floor_features is the always-on Safari-15 baseline. It covers everything
// the emitter produces today: ref.func/ref.null, memory.copy/fill/init,
// multi-return, sign_extendN, the mutable __vsp/__heap_base globals, and
// saturating (nontrapping) float->int casts.
const wasm_floor_features = [WasmFeature.reference_types, .bulk_memory, .multivalue, .sign_ext,
	.mutable_globals, .nontrapping_f2i]

// wasm_optin_defines maps a `-d <name>` compile define to the feature it enables.
const wasm_optin_defines = {
	'wasm_gc':             WasmFeature.gc
	'wasm_exceptions':     WasmFeature.exception_handling
	'wasm_tail_call':      WasmFeature.tail_call
	'wasm_simd':           WasmFeature.simd
	'wasm_threads':        WasmFeature.threads
	'wasm_memory64':       WasmFeature.memory64
	'wasm_relaxed_simd':   WasmFeature.relaxed_simd
	'wasm_extended_const': WasmFeature.extended_const
}

struct FeatureFlag {
	binaryen        string // wasm-opt flag, e.g. '--enable-reference-types'
	wabt            string // WABT feature token, e.g. 'reference-types'
	wabt_default_on bool   // whether wasm-validate enables it without any flag
}

fn (f WasmFeature) flag() FeatureFlag {
	return match f {
		.reference_types {
			FeatureFlag{
				binaryen:        '--enable-reference-types'
				wabt:            'reference-types'
				wabt_default_on: true
			}
		}
		.bulk_memory {
			FeatureFlag{
				binaryen:        '--enable-bulk-memory'
				wabt:            'bulk-memory'
				wabt_default_on: true
			}
		}
		.multivalue {
			FeatureFlag{
				binaryen:        '--enable-multivalue'
				wabt:            'multi-value'
				wabt_default_on: true
			}
		}
		.sign_ext {
			FeatureFlag{
				binaryen:        '--enable-sign-ext'
				wabt:            'sign-extension'
				wabt_default_on: true
			}
		}
		.mutable_globals {
			FeatureFlag{
				binaryen:        '--enable-mutable-globals'
				wabt:            'mutable-globals'
				wabt_default_on: true
			}
		}
		.nontrapping_f2i {
			FeatureFlag{
				binaryen:        '--enable-nontrapping-float-to-int'
				wabt:            'saturating-float-to-int'
				wabt_default_on: true
			}
		}
		.simd {
			FeatureFlag{
				binaryen:        '--enable-simd'
				wabt:            'simd'
				wabt_default_on: true
			}
		}
		.gc {
			FeatureFlag{
				binaryen:        '--enable-gc'
				wabt:            'gc'
				wabt_default_on: false
			}
		}
		.exception_handling {
			FeatureFlag{
				binaryen:        '--enable-exception-handling'
				wabt:            'exceptions'
				wabt_default_on: false
			}
		}
		.tail_call {
			FeatureFlag{
				binaryen:        '--enable-tail-call'
				wabt:            'tail-call'
				wabt_default_on: false
			}
		}
		.threads {
			FeatureFlag{
				binaryen:        '--enable-threads'
				wabt:            'threads'
				wabt_default_on: false
			}
		}
		.memory64 {
			FeatureFlag{
				binaryen:        '--enable-memory64'
				wabt:            'memory64'
				wabt_default_on: false
			}
		}
		.relaxed_simd {
			FeatureFlag{
				binaryen:        '--enable-relaxed-simd'
				wabt:            'relaxed-simd'
				wabt_default_on: false
			}
		}
		.extended_const {
			FeatureFlag{
				binaryen:        '--enable-extended-const'
				wabt:            'extended-const'
				wabt_default_on: false
			}
		}
	}
}

// enabled_wasm_features returns the floor set plus any opt-in feature whose
// `-d` define was passed on the command line.
fn (g &Gen) enabled_wasm_features() []WasmFeature {
	mut feats := wasm_floor_features.clone()
	for define, feat in wasm_optin_defines {
		if define in g.pref.compile_defines {
			feats << feat
		}
	}
	return apply_feature_implications(feats)
}

// apply_feature_implications expands `feats` with any feature implied by another.
// Relaxed SIMD extends the SIMD/v128 feature, so requesting it must also enable
// SIMD. Otherwise wabt_validate_args() emits `--disable-simd --enable-relaxed-simd`
// and wasm-opt runs from `-mvp` without `--enable-simd`, so a `-d wasm_relaxed_simd`
// build still fails under `-wasm-validate`/`-prod`.
fn apply_feature_implications(feats []WasmFeature) []WasmFeature {
	mut res := feats.clone()
	if WasmFeature.relaxed_simd in res && WasmFeature.simd !in res {
		res << .simd
	}
	return res
}

// binaryen_feature_flags renders the feature set into `wasm-opt` flags. It
// starts from `-mvp` (all non-MVP features off) and enables only the allowlist,
// so emitting an opcode outside the set makes wasm-opt itself fail rather than
// silently optimise it through.
fn binaryen_feature_flags(feats []WasmFeature) string {
	mut flags := ['-mvp']
	for feat in feats {
		flags << feat.flag().binaryen
	}
	return flags.join(' ')
}

// wabt_validate_args renders the feature set into `wasm-validate` arguments,
// relative to WABT's defaults: disable any default-on feature not in the set
// (e.g. `--disable-simd` on a floor build), and enable any default-off feature
// that is in the set. This gives the validator teeth - a stray above-floor
// opcode fails validation instead of passing under WABT's permissive defaults.
fn wabt_validate_args(feats []WasmFeature) []string {
	mut args := []string{}
	for feat in wasm_all_features {
		ff := feat.flag()
		enabled := feat in feats
		if ff.wabt_default_on && !enabled {
			args << '--disable-${ff.wabt}'
		} else if !ff.wabt_default_on && enabled {
			args << '--enable-${ff.wabt}'
		}
	}
	return args
}
