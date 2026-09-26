module wasm

fn test_relaxed_simd_implies_simd() {
	// requesting relaxed SIMD on its own must also pull in the base SIMD feature
	feats := apply_feature_implications([WasmFeature.relaxed_simd])
	assert WasmFeature.simd in feats
	assert WasmFeature.relaxed_simd in feats
}

fn test_relaxed_simd_does_not_duplicate_simd() {
	// when SIMD is already present, the implication must not add a duplicate
	feats := apply_feature_implications([WasmFeature.simd, .relaxed_simd])
	assert feats.filter(it == WasmFeature.simd).len == 1
}

fn test_simd_without_relaxed_is_unchanged() {
	feats := apply_feature_implications([WasmFeature.simd])
	assert WasmFeature.simd in feats
	assert WasmFeature.relaxed_simd !in feats
}

fn test_relaxed_simd_renders_simd_in_tool_flags() {
	feats := apply_feature_implications([WasmFeature.relaxed_simd])
	// wasm-opt must be told to enable SIMD as well as relaxed SIMD
	binaryen := binaryen_feature_flags(feats)
	assert binaryen.contains('--enable-simd')
	assert binaryen.contains('--enable-relaxed-simd')
	// wasm-validate must not disable SIMD while enabling relaxed SIMD
	wabt := wabt_validate_args(feats)
	assert '--disable-simd' !in wabt
	assert '--enable-relaxed-simd' in wabt
}
