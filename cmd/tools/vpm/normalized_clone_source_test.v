module main

import os

// The stored origin of a module installed on Windows is a `file:///S:/...` URL.
// It must normalise to the same value as the plain path, or pins are never matched.
fn test_normalized_clone_source_matches_windows_drive_file_urls() {
	$if windows {
		path := os.getwd()
		url := 'file:///' + path.replace('\\', '/')
		normalized := normalized_clone_source(url)
		assert normalized == normalized_clone_source(path)
		assert normalized.all_after('file://').count(':') == 1, normalized
	}
}
