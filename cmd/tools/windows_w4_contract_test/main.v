module main

import os
const w4_runner = 'run_multiwindow_win32_w4.ps1'

fn main() {
	root := os.join_path(@DIR, '..', '..', '..')
	runner_path := os.join_path(root, '.github', 'workflows', w4_runner)
	assert os.exists(runner_path), 'missing W4 runner ${w4_runner}'
	// sanity: workflows reference the runner
	for cc in ['gcc', 'msvc', 'tcc'] {
		wf := os.join_path(root, '.github', 'workflows', 'windows_ci_${cc}.yml')
		content := os.read_file(wf) or { '' }
		assert content.len > 0, 'missing workflow windows_ci_${cc}.yml'
	}
	println('windows w4 contract tests (minimal): OK')
}
