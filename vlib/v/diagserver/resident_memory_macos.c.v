module diagserver

// The import lives here, not in diagserver_nix.c.v, so that other Unix builds do
// not import a module they never use (an error with `-W`).
import runtime

// macos_resident_kb returns the memory this process uses, in KiB.
fn macos_resident_kb() ?i64 {
	bytes := runtime.used_memory() or { return none }
	return i64(bytes / 1024)
}
