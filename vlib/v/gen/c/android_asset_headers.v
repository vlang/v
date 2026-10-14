module c

fn (g &FlatGen) is_android_asset_binding(source_file string) bool {
	root := g.compiler_vroot.replace('\\', '/').trim_right('/')
	if root.len == 0 {
		return false
	}
	if source := g.a.cached_header_sources[source_file] {
		// A module cache header combines the os declarations and records its
		// first source file, rather than the asset binding's original filename.
		return source.replace('\\', '/').starts_with(root + '/vlib/os/')
	}
	return source_file.replace('\\', '/') == root + '/vlib/os/os_android_outside_termux.c.v'
}

fn (g &FlatGen) omits_unused_android_asset_sdk() bool {
	if g.target.os != 'android' || g.no_builtin || g.output_cross_c || g.object_file_mode
		|| g.target_libc_headers || !g.has_used_fn_filter() {
		return false
	}
	for name, _ in g.used_fns {
		if android_asset_reference(name) {
			return false
		}
	}
	mut binding_file := false
	for node in g.a.nodes {
		if node.kind == .file {
			binding_file = g.is_android_asset_binding(node.value)
			continue
		}
		// The binding itself declares every asset type, even when none is used.
		// Other files conservatively retain the SDK for type-only and enum uses,
		// as well as calls inside functions that reachability has not removed yet.
		if binding_file || node.kind in [.string_literal, .string_interp, .directive] {
			continue
		}
		if android_asset_reference(node.value) || android_asset_reference(node.typ) {
			return false
		}
	}
	return true
}

fn android_asset_reference(value string) bool {
	return value.contains('Asset') || value.contains('NativeActivity')
		|| value.contains('read_apk_asset') || value.contains('AASSET_')
}
