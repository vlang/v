module parser

import v.ast
import v.checker
import v.pref

fn test_parenthesized_propagated_index_can_be_followed_by_or_block() {
	source_text := "
fn main() {
	values := [?string('known'), ?string(none)]
	_ = (values[1]!) or { 'unknown' }
}
"
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut prog := parse_text(source_text, '', mut table, .skip_comments, vpref)
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)
}

fn test_top_level_comptime_else_allows_one_line_import() {
	source_text := "
$if never_defined ? {} $else { import log }

log.info('42')
"
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut prog := parse_text(source_text, '', mut table, .skip_comments, vpref)
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)
}
