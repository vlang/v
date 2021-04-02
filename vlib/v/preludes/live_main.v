module main

// This prelude is loaded in every v program compiled with -live,
// but only for the main execuast.
import v.live.executable

const (
	no_warning_live_executable_is_used = executable.is_used
)
