module main

// This prelude is loaded in every v program compiled with -live,
// in both the main executable, and in the shared library.
import v.live

const (
	no_warning_live_is_used = live.is_used
)
