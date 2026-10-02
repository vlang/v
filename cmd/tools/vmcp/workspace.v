// Workspace is the state every tool in this server shares: where the compiler
// lives, which project it is pointed at, and whether it may write anything.
//
// It is created once at startup and then only read, so a tool cannot change the
// root another tool resolves paths against halfway through a request.
module main

import os
import v.skills
import v.util
import v.vmod

// server_name and server_version identify this server in the MCP handshake.
pub const server_name = 'v.mcp'
pub const server_version = '1.0.0'

// read_only_root is the file name that pins a checkout to inspection only. It
// lives next to `v.mod` and is honoured by the module search, so this tool reads
// the same project boundary the compiler does.
pub const read_only_root = '.v.mcp.readonly'

// Workspace is the shared, immutable-per-request state of the server.
pub struct Workspace {
pub:
	// vroot is the V source tree this compiler was built from. It anchors the
	// bundled skills and the bundled documentation.
	vroot string
	// compiler is the `v` executable to invoke for `run`, `test` and `doctor`.
	compiler string
	// root is the directory every relative path in a request resolves against.
	// It is the directory the server was started in, unless `--root` said
	// otherwise.
	root string
	// project_root is the nearest directory holding a `v.mod`, or `root` when
	// there is none.
	project_root string
	// read_only keeps every tool that writes a file unregistered.
	read_only bool
	// v_modified reports whether `v.mod` was found.
	v_modified bool
	// v_mod_file is the `v.mod` this project was read from.
	v_mod_file string
	// is_v_checkout reports whether `root` is the V compiler's own source tree.
	is_v_checkout bool
pub mut:
	// The fields below are filled once, by `new_workspace`, and only read
	// afterwards. They are mutable so the constructor can fill them in place.
	// dependencies are the module names listed in `v.mod`.
	dependencies []string
	// v_mod_name, v_mod_description, v_mod_version, v_mod_license and
	// v_mod_repo_url come from that file.
	v_mod_name        string
	v_mod_description string
	v_mod_version     string
	v_mod_license     string
	v_mod_repo_url    string
}

// new_workspace reads the project state under `root`.
pub fn new_workspace(vroot string, root string, read_only bool) Workspace {
	real_root := os.real_path(root)
	project_root := util.nearest_vmod_root(real_root) or { real_root }
	v_mod_file := os.join_path(project_root, 'v.mod')
	mut ws := Workspace{
		vroot:         vroot
		compiler:      compiler_exe()
		root:          real_root
		project_root:  project_root
		read_only:     read_only
		v_modified:    os.is_file(v_mod_file)
		v_mod_file:    v_mod_file
		is_v_checkout: project_root == os.real_path(vroot)
	}
	if ws.v_modified {
		read_v_mod(mut ws)
	}
	return ws
}

// read_v_mod fills the manifest fields from the project's `v.mod`.
//
// A manifest the parser rejects leaves every field empty rather than failing the
// server: the rest of the tools still work on a project whose `v.mod` is
// temporarily broken, which is exactly when an agent needs them.
fn read_v_mod(mut ws Workspace) {
	manifest := vmod_from_file(ws.v_mod_file) or { return }
	ws.v_mod_name = manifest.name
	ws.v_mod_description = manifest.description
	ws.v_mod_version = manifest.version
	ws.v_mod_license = manifest.license
	ws.v_mod_repo_url = manifest.repo_url
	for dependency in manifest.dependencies {
		ws.dependencies << dependency
	}
}

// vmod_from_file reads and decodes a `v.mod`.
fn vmod_from_file(path string) ?vmod.Manifest {
	contents := os.read_file(path) or { return none }
	return vmod.decode(contents)
}

// compiler_exe returns the `v` executable to invoke.
//
// `VEXE` is what the launcher sets for the tool it is running, so a tool spawned
// by `v mcp ...` calls the very same compiler the user typed. Falling back to
// the running executable keeps the server working when it is launched directly.
fn compiler_exe() string {
	return os.getenv_opt('VEXE') or { os.real_path(os.executable()) }
}

// resolve turns a path from a request into an absolute path inside the workspace
// root.
//
// A relative path resolves against `root`. An absolute path is accepted only
// when it stays inside `root`, so a request cannot reach a file outside the
// project it was pointed at.
pub fn (ws &Workspace) resolve(path string) !string {
	if path == '' {
		return error('a path is required')
	}
	full := if os.is_abs_path(path) {
		canonical_request_path(path)!
	} else {
		canonical_request_path(os.join_path(ws.root, path))!
	}
	if !is_inside(ws.root, full) {
		return error('`${path}` is outside the workspace root `${ws.root}`')
	}
	return full
}

// canonical_request_path resolves the existing ancestor of a possibly new file.
// real_path alone leaves unresolved .. segments and symlink parents when the
// final file does not exist, which is common for editing tools that create files.
fn canonical_request_path(path string) !string {
	mut ancestor := path
	mut missing := []string{}
	for !os.exists(ancestor) {
		if os.is_link(ancestor) {
			return error('cannot resolve dangling symlink `${ancestor}`')
		}
		name := os.file_name(ancestor)
		// Backtracking through a missing directory cannot be resolved physically.
		// Normalizing it could expose an existing symlink without checking its target.
		if name in ['.', '..'] {
			return error('cannot resolve path `${path}` through a missing directory')
		}
		parent := os.dir(ancestor)
		if parent == ancestor {
			return error('cannot resolve path `${path}`')
		}
		missing << name
		ancestor = parent
	}
	mut full := os.real_path(ancestor)
	for i := missing.len - 1; i >= 0; i-- {
		full = os.join_path(full, missing[i])
	}
	return os.norm_path(full)
}

// is_inside reports whether `path` is `base` or lies below it.
fn is_inside(base string, path string) bool {
	if path == base {
		return true
	}
	separator := if base.ends_with('/') || base.ends_with('\\') { '' } else { path_separator }
	return path.starts_with(base + separator)
}

// path_separator is the separator of the running platform. `os.join_path` and
// `os.real_path` both emit it, so comparing with it is exact.
const path_separator = if os.user_os() == 'windows' { '\\' } else { '/' }

// relative renders `path` the way tool output reports it: relative to the
// workspace root with forward slashes.
pub fn (ws &Workspace) relative(path string) string {
	return skills.relative_to(ws.root, path)
}

// v_files returns the project's V sources below `dir`, sorted, excluding the
// directories a project never compiles from.
pub fn (ws &Workspace) v_files(dir string) []string {
	mut files := []string{}
	for path in os.walk_ext(dir, '.v', os.WalkParams{}) {
		if ws.is_ignored(path) {
			continue
		}
		files << path
	}
	for path in os.walk_ext(dir, '.vsh', os.WalkParams{}) {
		if ws.is_ignored(path) {
			continue
		}
		files << path
	}
	files.sort()
	return files
}

// ignored_dirs are the directories a V project never compiles from.
const ignored_dirs = ['.git', '.vmodules', 'node_modules', 'target', 'bin', 'dist', '.agents',
	'.opencode', '.claude', '.cursor']

// is_ignored reports whether a path sits in a directory no tool should read.
fn (ws &Workspace) is_ignored(path string) bool {
	relative := ws.relative(path)
	for segment in relative.split('/') {
		if segment in ignored_dirs {
			return true
		}
	}
	return false
}
