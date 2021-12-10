module main

// This prelude is loaded in every v program compiled with -live,
// but only for the shared library.
import v.live.sharedlib

const (
	no_warning_live_shared_is_used = sharedlib.is_used
)
