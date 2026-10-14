import os.font

// Font families covering each naming scheme the variant lookup knows: a
// `-Regular` stem, a DejaVu-style bare stem, and Arial's own suffixes. These
// are plain strings - nothing here touches the disk.
const sans_regular = 'C:\\Windows\\Fonts\\NotoSans-Regular.ttf'
const dejavu = '/usr/share/fonts/DejaVuSans.ttf'
const arial = '/fonts/Arial.ttf'

fn test_normal_variant_leaves_a_real_path_alone() {
	for p in [sans_regular, dejavu, arial, 'x.ttf'] {
		assert font.get_path_variant(p, .normal) == p
	}
}

fn test_variant_numbers_follow_the_declaration_order() {
	assert int(font.Variant.normal) == 0
	assert int(font.Variant.bold) == 1
	assert int(font.Variant.mono) == 2
	assert int(font.Variant.italic) == 3
}

fn test_bare_stem_gets_a_suffix_appended() {
	assert font.get_path_variant(dejavu, .bold) == '/usr/share/fonts/DejaVuSans-Bold.ttf'
	assert font.get_path_variant(dejavu, .italic) == '/usr/share/fonts/DejaVuSans-Oblique.ttf'
	assert font.get_path_variant(dejavu, .mono) == '/usr/share/fonts/DejaVuSansMono.ttf'
}

fn test_arial_uses_its_own_suffixes_and_has_no_mono() {
	assert font.get_path_variant(arial, .bold) == '/fonts/Arialbd.ttf'
	assert font.get_path_variant(arial, .italic) == '/fonts/Ariali.ttf'
	// Arial has no mono variant, so the request is a deliberate no-op
	assert font.get_path_variant(arial, .mono) == arial
	// the family check lowercases the stem first, so casing does not matter
	assert font.get_path_variant('/fonts/arial.ttf', .mono) == '/fonts/arial.ttf'
	assert font.get_path_variant('/fonts/ARIAL.ttf', .bold) == '/fonts/ARIALbd.ttf'
}

fn test_extension_and_directory_are_preserved() {
	for variant in [font.Variant.bold, .italic, .mono] {
		out := font.get_path_variant(sans_regular, variant)
		assert out.ends_with('.ttf')
		assert out.starts_with('C:\\Windows\\Fonts\\')
	}
	// a path with no directory at all keeps its bare name
	assert font.get_path_variant('x.ttf', .bold) == 'x-bold.ttf'
	assert font.get_path_variant('x.ttf', .italic) == 'xItalic.ttf'
	assert font.get_path_variant('x.ttf', .mono) == 'xMono.ttf'
}

// NOTE: the `.bold` arm at vlib/os/font/font.v:196 tests `fpath.ends_with('-Regular')`,
// where `fpath` is the *directory* half of the path and always ends in a
// separator (or is empty), so that branch can never fire. The `.italic` arm at
// :212 and the `.mono` arm at :223 both test `file` correctly. The result is
// that a `-Regular` stem only gets its `-Bold` replacement by accident - it does
// not - and falls through to the generic `-bold` suffix. Current behaviour is
// asserted here; correcting it is a behaviour change and was left alone.
fn test_regular_stem_is_replaced_for_italic_and_mono_but_not_bold() {
	assert font.get_path_variant(sans_regular, .italic) == 'C:\\Windows\\Fonts\\NotoSans-Italic.ttf'
	assert font.get_path_variant(sans_regular, .mono) == 'C:\\Windows\\Fonts\\NotoSansMono-Regular.ttf'
	assert font.get_path_variant(sans_regular, .bold) == 'C:\\Windows\\Fonts\\NotoSans-Regular-bold.ttf'
	assert font.get_path_variant('/usr/share/fonts/LiberationSans-Regular.ttf', .bold) == '/usr/share/fonts/LiberationSans-Regular-bold.ttf'
}

// NOTE: `Arial` is matched through `flower` (the lowercased stem), so the family
// check is case-insensitive, but the DejaVu check at :198 is not, and neither
// is the `.ttf` suffix strip at :188 - only lowercase `.ttf` is removed, so an
// uppercase extension survives and a second one is appended.
fn test_arial_matching_ignores_case_on_the_family_name() {
	assert font.get_path_variant('/fonts/ARIAL.ttf', .bold) == '/fonts/ARIALbd.ttf'
	assert font.get_path_variant('/fonts/DEJAVUSANS.ttf', .bold) == '/fonts/DEJAVUSANS-bold.ttf'
	assert font.get_path_variant('/fonts/ARIAL.TTF', .bold) == '/fonts/ARIAL.TTFbd.ttf'
	assert font.get_path_variant('/fonts/ARIAL.TTF', .mono) == '/fonts/ARIAL.TTF.ttf'
}

// NOTE: get_path_variant is documented as returning the variant path "if it
// exists", but it never touches the filesystem - it only builds a name. These
// cases show it running on an empty stem.
fn test_an_empty_stem_still_produces_a_name() {
	assert font.get_path_variant('', .normal) == '.ttf'
	assert font.get_path_variant('', .mono) == 'Mono.ttf'
	assert font.get_path_variant('', .bold) == '-bold.ttf'
}
