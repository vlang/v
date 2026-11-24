module diagserver

// The import lives here, not in diagserver_nix.c.v, where its only use is inside
// `$if macos`: a Linux build would import a module it never uses (an error with `-W`).
// This file is not `_macos.c.v`: portable C, generated for every Unix at once, keeps
// that `$if macos` branch, and calls this function there.
import runtime

// macos_resident_kb returns the memory this process uses, in KiB.
fn macos_resident_kb() ?i64 {
	bytes := runtime.used_memory() or { return none }
	return i64(bytes / 1024)
}
