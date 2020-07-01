module main

// This prelude is loaded in every v program compiled with -live,
// but only for the shared library.
import live.shared

const (
	no_warning_live_shared_is_used = shared.is_used
)
