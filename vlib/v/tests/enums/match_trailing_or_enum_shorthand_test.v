// `match x {...} or {...}` unwraps the option subject `x` with the `or` block;
// the match itself is the value, so enum shorthands in its branches take the
// type the context expects.
enum Tristate {
	true_
	false_
	mixed
	undefined
}

enum Live {
	assertive
	off
	polite
}

fn parse_tristate(value ?string) Tristate {
	return match value {
		'true' { .true_ }
		'false' { .false_ }
		'mixed' { .mixed }
		else { .undefined }
	} or { return .undefined }
}

fn parse_live(value ?string) ?Live {
	return match value {
		'assertive' { .assertive }
		'off' { .off }
		'polite' { .polite }
		else { none }
	} or { return none }
}

fn parse_bool(value ?string) ?bool {
	return match value {
		'true' { true }
		'false' { false }
		else { none }
	} or { return none }
}

fn tristate_name(t Tristate) string {
	return t.str()
}

fn test_return_match_trailing_or_enum_shorthand() {
	assert parse_tristate('true') == .true_
	assert parse_tristate('mixed') == .mixed
	assert parse_tristate('other') == .undefined
	assert parse_tristate(none) == .undefined
}

fn test_return_option_match_trailing_or() {
	assert parse_live('off')? == .off
	assert parse_live('polite')? == .polite
	assert parse_live('other') == none
	assert parse_live(none) == none
	assert parse_bool('true')? == true
	assert parse_bool('false')? == false
	assert parse_bool('other') == none
	assert parse_bool(none) == none
}

fn test_match_trailing_or_in_assignment_and_argument() {
	missing := ?string(none)
	mut t := Tristate.undefined
	t = match missing {
		'mixed' { .mixed }
		else { .false_ }
	} or { 'mixed' }
	assert t == .mixed
	assert tristate_name(match missing {
		'true' { .true_ }
		else { .false_ }
	} or { 'false' }) == 'false_'
}
