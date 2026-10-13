module main

// The stored origin of a module installed on Windows is a `file:///S:/...` URL.
// It must normalise to the same value as the plain path, or pins are never matched.
fn test_normalized_clone_source_matches_windows_drive_file_urls() {
	$if windows {
		path := @DIR
		url := 'file:///' + path.replace('\\', '/')
		normalized := normalized_clone_source(url)
		assert normalized == normalized_clone_source(path)
		assert normalized.all_after('file://').count(':') == 1, normalized
	}
}

// On POSIX, `/a:/...` is an ordinary path, not a drive path. Stripping its slash
// would make it relative, so the same URL would normalise differently from each
// working directory.
fn test_normalized_clone_source_keeps_posix_paths_with_a_colon_after_one_character() {
	$if !windows {
		assert normalized_clone_source('file:///a:/repo') == 'file:///a:/repo'
	}
}
