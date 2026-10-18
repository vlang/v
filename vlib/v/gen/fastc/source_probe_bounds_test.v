module fastc

import v.pref
import v.scanner
import v.token

fn test_fastc_native_context_word_gate_preserves_lexical_candidates() {
	prefs := pref.new_preferences()
	for source in [
		'',
		'module main\nfn main() { valueC := 1\n _ = valueC }',
		r'module main
// C.comment_ref() #include "comment.h"
/* C.block_ref() */
fn main() { text := "C.string_ref() #include missing.h" }
',
		r'module main
fn main() { text := r"${C.raw_string_ref()}" }
',
		'module main\nfn main() { @C := 1 }',
	] {
		mut references := map[string]bool{}
		mut declarations := map[string]bool{}
		assert !fastc_target_collect_native_context(source, 'native_probe.v', prefs,
			mut references, mut declarations), source
		assert references.len == 0, references.str()
		assert declarations.len == 0, declarations.str()
	}
}

fn test_fastc_native_context_keeps_declarations_keywords_comments_and_interpolation() {
	prefs := pref.new_preferences()
	source := r'module main
fn C.owned_decl() int
fn main() {
	C/* qualifier comment */.native_ref()
	C.volatile()
	text := "${C.interpolated_ref()}"
}
'
	mut references := map[string]bool{}
	mut declarations := map[string]bool{}
	assert !fastc_target_collect_native_context(source, 'native_probe.v', prefs,
		mut references, mut declarations)
	assert references == {
		'native_ref':       true
		'volatile':         true
		'interpolated_ref': true
	}
	assert declarations == {
		'owned_decl': true
	}
}

fn test_fastc_native_context_keeps_selected_directives_without_opening_headers() {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('macos', 'arm64')!
	for directive in ['#include "does_not_exist.h"', '# preinclude "does_not_exist.h"',
		'#insert macos "does_not_exist.h"'] {
		mut references := map[string]bool{}
		mut declarations := map[string]bool{}
		assert fastc_target_collect_native_context('module main\n${directive}\n',
			'native_probe.v', prefs, mut references, mut declarations), directive
		assert references.len == 0
		assert declarations.len == 0
	}
	source := r'module main
$if true {
	#include "does_not_exist.h"
	fn C.selected_decl()
	fn selected() { C.selected_ref() }
} $else {
	fn C.unselected_decl()
	fn unselected() { C.unselected_ref() }
}
'
	mut references := map[string]bool{}
	mut declarations := map[string]bool{}
	assert fastc_target_collect_native_context(source, 'native_probe.v', prefs,
		mut references, mut declarations)
	assert references == {
		'selected_ref': true
	}
	assert declarations == {
		'selected_decl': true
	}
}

fn test_fastc_channel_probe_bound_preserves_token_boundaries_and_interpolation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	header := FastcSourceHeader{ module_name: 'main' }
	for source in [
		'',
		'channel chan2 _chan',
		'@chan',
		'// chan\nfn unrelated() { println("tail") }\n',
		'/* chan */\nfn unrelated() { println("tail") }\n',
		'text := "chan int"\nfn unrelated() { println("tail") }\n',
		r'text := r"${chan int{}}"',
	] {
		result := fastc_header_with_channel_runtime(header, source, prefs)
		assert '__fastc_channels' !in result.imports, source
		assert 'sync' !in result.import_order, source
	}
	for source in [
		'chan',
		'items := chan/* type */int{}\nfn unrelated() { println("tail") }\n',
		'items := chan\nint{}\n// later chan string candidate\n',
		r'text := "${chan int{}}"',
		'import chan\nfn unrelated() {}\n',
	] {
		result := fastc_header_with_channel_runtime(header, source, prefs)
		assert result.imports['__fastc_channels'] == 'sync', source
		assert result.import_order == ['sync'], source
	}
}

// Keep the former unbounded probes as a differential oracle for lexical
// fragments. Every byte prefix also covers truncated tokens and interpolation.
fn fastc_probe_unbounded_native_context(source string, path string, prefs &pref.Preferences, mut references map[string]bool, mut declarations map[string]bool) bool {
	mut scan := scanner.new_scanner(prefs, .normal)
	file := token.File.unindexed(path, source.len)
	scan.init(file, source)
	mut previous := token.Token.unknown
	mut has_header := false
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .dollar {
			mut selected_scan := scan
			if selected_scan.scan() == .key_if {
				if selected := fastc_scan_selected_comptime_branch(mut selected_scan, .key_if, path, prefs) {
					if selected.source != '' {
						has_header = fastc_probe_unbounded_native_context(selected.source, path, prefs, mut references, mut declarations) || has_header
					}
					scan = selected_scan
					tok = selected.tok
					previous = .unknown
					continue
				}
			}
		}
		if tok == .hash {
			directive := scan.lit.trim_space().fields()
			if directive.len > 0 && directive[0] in ['include', 'preinclude', 'insert'] {
				qualifier := if directive.len > 1 { directive[1] } else { '' }
				if qualifier !in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd',
					'dragonfly', 'solaris', 'android'] || pref.comptime_flag_value(prefs, qualifier) {
					has_header = true
				}
			}
		}
		if tok == .name && scan.lit == 'C' && previous != .key_fn {
			mut look := scan
			if look.scan() == .dot {
				name_token := look.scan()
				if name_token == .name || name_token.is_keyword() {
					references[look.lit] = true
				}
			}
		}
		if tok == .key_fn {
			mut look := scan
			if look.scan() == .name && look.lit == 'C' && look.scan() == .dot {
				name_token := look.scan()
				if name_token == .name || name_token.is_keyword() {
					declarations[look.lit] = true
				}
			}
		}
		previous = tok
		tok = scan.scan()
	}
	return has_header
}

fn fastc_probe_unbounded_channel_runtime(header FastcSourceHeader, source string, prefs &pref.Preferences) FastcSourceHeader {
	if !prefs.building_v || header.module_name in ['sync', 'builtin']
		|| !fastc_source_contains_word(source, 'chan') {
		return header
	}
	mut scan := scanner.new_scanner(prefs, .normal)
	file := token.File.unindexed('channel_runtime.v', source.len)
	scan.init(file, source)
	mut has_channel := false
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .name && scan.lit == 'chan' {
			has_channel = true
			break
		}
		tok = scan.scan()
	}
	if !has_channel { return header }
	mut imports := header.imports.clone()
	mut order := header.import_order.clone()
	imports['__fastc_channels'] = 'sync'
	if 'sync' !in order { order << 'sync' }
	return FastcSourceHeader{ ...header, imports: imports, import_order: order }
}

fn test_fastc_source_probes_match_unbounded_scanning_on_every_byte_prefix() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('macos', 'arm64')!
	header := FastcSourceHeader{ module_name: 'main' }
	seeds := [
		'@C @chan C2 chan2 _C _chan C/* comment */.volatile() chan/* type */int{}',
		'/* outer /* nested */ end */ C.live() // chan fake\n chan int{}',
		'/* /*/ C.live() /* //*/ chan int{}',
		'\xef\xbb\xbfchan int{}\n C.live()',
		'échan int{} C.live()',
		r'text := "${C.live()} ${chan int{}}"; raw := r"${C.ignored()} chan"',
		r'text := "${"nested ${C.inner()}"} chan"; C.tail()',
		r'// C.ignored() chan
#include windows "unopened.h"
# preinclude "unopened.h"
fn C.declared()
fn main() { C.called() }
',
		r'$if true {
	$if false { fn dead() { C.dead(); chan int{} } }
	$else { fn live() { C.live() } }
} $else { #insert "unopened.h" }
',
	]
	for seed in seeds {
		for end in 0 .. seed.len + 1 {
			source := seed[..end]
			mut actual_refs := map[string]bool{}
			mut actual_decls := map[string]bool{}
			mut expected_refs := map[string]bool{}
			mut expected_decls := map[string]bool{}
			actual_native := fastc_target_collect_native_context(source, 'prefix_probe.v', prefs,
				mut actual_refs, mut actual_decls)
			expected_native := fastc_probe_unbounded_native_context(source, 'prefix_probe.v', prefs,
				mut expected_refs, mut expected_decls)
			assert actual_native == expected_native, source
			assert actual_refs == expected_refs, source
			assert actual_decls == expected_decls, source
			actual_channel := fastc_header_with_channel_runtime(header, source, prefs)
			expected_channel := fastc_probe_unbounded_channel_runtime(header, source, prefs)
			assert actual_channel.imports == expected_channel.imports, source
			assert actual_channel.import_order == expected_channel.import_order, source
		}
	}
}
