module ssa

// test_bench_runtime_stubs_include_macos_rss_helper validates this v3 regression case.
fn test_bench_runtime_stubs_include_macos_rss_helper() {
	assert 'macos_rss_kb' in bench_runtime_stub_names
	assert 'bench.macos_rss_kb' in bench_runtime_stub_names
	assert 'macos_peak_rss_kb' !in bench_runtime_stub_names
	assert 'bench.macos_peak_rss_kb' !in bench_runtime_stub_names
	b := Builder{}
	assert b.skip_source_fn('macos_rss_kb')
	assert b.skip_source_fn('bench.macos_rss_kb')
}
