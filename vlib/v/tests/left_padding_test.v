module main

fn test_main() {
	// vfmt off
	assert '[${8:-010}]' == '[8000000000]'
	assert '[${8:010}]' == '[0000000008]'
	assert '[${8:-10}]' == '[8         ]'
	assert '[${8:10}]' == '[         8]'
	// vfmt on
}
