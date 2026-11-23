module fastc

import os
import strings
import v.pref

// FastcZygoteProcess is a copy of the compiler process that holds one TinyCC
// state, with the compiler's ends of its pipes (see libtcc_zygote_darwin.c).
struct FastcZygoteProcess {
mut:
	pid            int
	control_fd     int
	diagnostics_fd int
	done_fd        int
	unit_fds       []int
	active         bool
	// Once started, the reader owns and closes diagnostics_fd.
	diagnostics_reader  thread string
	diagnostics_started bool
}

// FastcZygoteHead is the text a zygote parsed before the build knew its own:
// the TinyCC options and the head that the previous build of the program kept.
struct FastcZygoteHead {
	options string
	text    string
	fed     bool
}

// FastcZygote compiles the translation units of a program from one parse of
// the declarations that they share. A zygote process parses that head and then
// continues as one process per unit. The compiler forks its zygotes before it
// generates C, while its own address space is still small: `fresh` waits for
// the head of this build, and `guess` parses the head that the previous build
// kept in `head_path`, while this build is generated. When both heads are the
// same text, the units start from `guess` without waiting for a parse.
pub struct FastcZygote {
mut:
	fresh      FastcZygoteProcess
	guess      FastcZygoteProcess
	guess_feed thread FastcZygoteHead
	head_path  string
	active     bool
}

// FastcZygotePlan groups what the units of a zygote add to the shared head: the
// program's single definitions and the bodies of its source files. Both are
// lists of items, the definitions first, and a unit holds consecutive items.
pub struct FastcZygotePlan {
pub:
	paths []string
mut:
	// Unit g is compiled to `<object_prefix>g.o`.
	object_prefix string
	// unit_items[g] is the first item of unit g; the last entry is the item count.
	unit_items []int
	// solo_pieces[i] is the first piece of definition item i; the last entry is
	// the piece that ends the definitions.
	solo_pieces []int
	// definitions define what the head declares `extern`, in the first unit.
	definitions string
}

// fastc_zygote_solo_item_bytes is the least text that forms a definition item.
const fastc_zygote_solo_item_bytes = 16 * 1024

// fastc_zygote_unit_bytes is the least text that is worth a unit of its own: a
// unit costs a process and an object, whatever it holds.
const fastc_zygote_unit_bytes = 256 * 1024

// fastc_zygote_marker ends the declarations that the units of a zygote share.
// The unit count and the object prefix follow it on its line.
const fastc_zygote_marker = '#warning __v_fastc_zygote_units'

// fastc_zygote_head_bytes is the least head that is kept for the next build: a
// small program parses its head faster than a second zygote is worth.
const fastc_zygote_head_bytes = 1024 * 1024

// fastc_zygote_head_format starts the file that keeps a head for the next build.
const fastc_zygote_head_format = 'fastc-zygote-head-v1\n'

// fastc_zygote_unit_limit is the largest number of units a zygote compiles at
// once. A unit costs a zygote one more process, not one more parse of the head.
pub fn fastc_zygote_unit_limit(prefs &pref.Preferences) int {
	mut units := fastc_parallel_worker_limit(prefs)
	if units > 12 {
		units = 12
	}
	return units
}

// fastc_zygote_head_path names the file in which the builds of `input` by this
// compiler keep their head for the next build.
pub fn fastc_zygote_head_path(input string) string {
	identity := '${os.real_path(input)}\x00${os.executable()}'
	key := fastc_unit_cache_hash(identity, u64(0x3c6ef372fe94f82b))
	return os.join_path_single(os.vtmp_dir(), 'fastc_zygote_${key.hex()}.head')
}

// fastc_start_zygote forks the zygotes of a build, which keeps its head in
// `head_path` when that is not empty. A compiler without an in-process TinyCC
// returns an inactive zygote, as does a failed fork or `V3_FASTC_NO_ZYGOTE`.
pub fn fastc_start_zygote(max_units int, head_path string) FastcZygote {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		if os.getenv('V3_FASTC_NO_ZYGOTE') != '' {
			return FastcZygote{}
		}
		return fastc_start_libtcc_zygote(max_units, head_path)
	} $else {
		return FastcZygote{}
	}
}

// fastc_zygote_accepts reports whether the zygote can compile units with
// `compile_args`. Its units part at a `#warning`, which these options would
// suppress or turn into an error.
pub fn fastc_zygote_accepts(zygote &FastcZygote, compile_args []string) bool {
	if !zygote.active {
		return false
	}
	for arg in compile_args {
		if arg == '-w' || arg == '-Werror' {
			return false
		}
	}
	return true
}

// fastc_zygote_definitions returns the definitions of the head that only one
// unit may hold: the external form of its `static` helpers and variables, and
// the embedded files that no separate object defines.
fn fastc_zygote_definitions(pieces []string, units &FastcUnitLayout) string {
	mut size := 0
	for e, index in units.extern_indexes {
		if units.define_texts[e] != units.extern_texts[e] {
			size += pieces[index].len
		}
	}
	mut out := strings.new_builder(size + 1)
	for e, index in units.extern_indexes {
		definition := units.define_texts[e]
		if definition == units.extern_texts[e] {
			continue
		}
		if definition.starts_with('const u8 __vf_embed_') {
			out.write_string(definition)
		} else {
			out.write_string(fastc_external_definitions(pieces[index]))
		}
	}
	return out.str()
}

// fastc_zygote_solo_pieces splits the definitions that follow the head into
// items of complete definitions, and appends their sizes to `sizes`.
fn fastc_zygote_solo_pieces(pieces []string, units &FastcUnitLayout, mut sizes []int) []int {
	mut starts := units.solo_starts.clone()
	mut ordered := starts.len > 0 && starts[0] == units.head_end
	for i in 1 .. starts.len {
		if starts[i] < starts[i - 1] || starts[i] > units.solo_end {
			ordered = false
		}
	}
	if !ordered {
		// A layout without group marks keeps its definitions together.
		starts = [units.head_end]
	}
	mut solo_pieces := []int{cap: starts.len + 1}
	mut item_start := units.head_end
	mut item_size := 0
	for i, start in starts {
		end := if i + 1 < starts.len { starts[i + 1] } else { units.solo_end }
		if item_size >= fastc_zygote_solo_item_bytes {
			solo_pieces << item_start
			sizes << item_size
			item_start = start
			item_size = 0
		}
		for k in start .. end {
			item_size += pieces[k].len
		}
	}
	solo_pieces << item_start
	sizes << item_size
	solo_pieces << units.solo_end
	return solo_pieces
}

// fastc_zygote_plan balances the definitions and the body units of a generation
// over at most `max_units` zygote units. It returns no paths when the program
// does not split.
pub fn fastc_zygote_plan(prefix string, pieces []string, units FastcUnitLayout, max_units int) FastcZygotePlan {
	unit_count := units.unit_starts.len - 1
	if max_units < 2 || unit_count < 2 || units.head_end <= 0 || units.solo_end > pieces.len
		|| units.head_end > units.solo_end {
		return FastcZygotePlan{}
	}
	mut sizes := []int{cap: unit_count + 64}
	solo_pieces := fastc_zygote_solo_pieces(pieces, &units, mut sizes)
	for u in 0 .. unit_count {
		mut size := 0
		for k in units.unit_starts[u] .. units.unit_starts[u + 1] {
			size += pieces[k].len
		}
		sizes << size
	}
	definitions := fastc_zygote_definitions(pieces, &units)
	mut total := definitions.len
	for size in sizes {
		total += size
	}
	mut groups := total / fastc_zygote_unit_bytes
	if groups > max_units {
		groups = max_units
	}
	if groups > sizes.len {
		groups = sizes.len
	}
	if groups < 2 {
		groups = 2
	}
	unit_items := fastc_unit_group_starts(sizes, groups, definitions.len + 1, 0)
	if unit_items.len == 0 {
		return FastcZygotePlan{}
	}
	// The marker line names the objects, so the prefix has to be one plain line.
	object_prefix := '${prefix}.unit'
	if object_prefix.len > 900 || object_prefix.contains_any('\r\n\\') || object_prefix.ends_with(' ') {
		return FastcZygotePlan{}
	}
	mut paths := []string{cap: groups}
	for g in 0 .. groups {
		paths << '${object_prefix}${g}.c'
	}
	return FastcZygotePlan{
		paths:         paths
		object_prefix: object_prefix
		unit_items:    unit_items
		solo_pieces:   solo_pieces
		definitions:   definitions
	}
}

// fastc_zygote_extern_slots maps every piece of the head to its entry in
// extern_texts, or to -1: every unit sees a definition that only one unit may
// hold as its `extern` declaration.
fn fastc_zygote_extern_slots(units &FastcUnitLayout) []int {
	mut slots := []int{len: units.head_end, init: -1}
	for e, index in units.extern_indexes {
		if index >= 0 && index < slots.len {
			slots[index] = e
		}
	}
	return slots
}

// fastc_zygote_head_matches reports whether `text` is the head of the generation.
fn fastc_zygote_head_matches(text string, pieces []string, units &FastcUnitLayout) bool {
	slots := fastc_zygote_extern_slots(units)
	mut offset := 0
	for k in 0 .. units.head_end {
		piece := if slots[k] >= 0 { units.extern_texts[slots[k]] } else { pieces[k] }
		if piece.len > text.len - offset {
			return false
		}
		if piece.len > 0 && unsafe { vmemcmp(text.str + offset, piece.str, piece.len) } != 0 {
			return false
		}
		offset += piece.len
	}
	return offset == text.len
}

// fastc_parse_zygote_head reads the options and the head text of a kept head.
fn fastc_parse_zygote_head(content string) ?FastcZygoteHead {
	if !content.starts_with(fastc_zygote_head_format) {
		return none
	}
	rest := content[fastc_zygote_head_format.len..]
	length_end := rest.index('\n') or { return none }
	options_len := rest[..length_end].int()
	if options_len < 0 || rest[..length_end] != options_len.str()
		|| options_len > rest.len - length_end - 1 {
		return none
	}
	return FastcZygoteHead{
		options: rest[length_end + 1..length_end + 1 + options_len]
		text:    rest[length_end + 1 + options_len..]
	}
}

// fastc_zygote_head_prefix precedes the head text in the file that keeps it.
fn fastc_zygote_head_prefix(options string) string {
	return '${fastc_zygote_head_format}${options.len}\n${options}'
}

// fastc_compile_zygote_c_units compiles the planned units to `objects` with the
// zygote, which is used up by this. With `preload_link` every object is loaded
// into the prepared linker, in unit order, as soon as it and those before it exist.
pub fn fastc_compile_zygote_c_units(mut zygote FastcZygote, base_args []string, objects []string, pieces []string, units FastcUnitLayout, plan FastcZygotePlan, mut prepared_link FastcPreparedLink, preload_link bool) ! {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		fastc_compile_libtcc_zygote_c_units(mut zygote, base_args, objects, pieces, units, plan, mut prepared_link, preload_link)!
	} $else {
		return error('the FastC unit zygote is unavailable')
	}
}

// fastc_discard_zygote ends a zygote that received no job and reaps it.
pub fn fastc_discard_zygote(mut zygote FastcZygote) {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		fastc_discard_libtcc_zygote(mut zygote)
	}
}
