module types

fn postfix_operator_end(src string) int {
	op := if src.contains('++') { '++' } else { '--' }
	return (src.index(op) or { -1 }) + op.len
}

// Like V1's parser, `)` or `]` after whitespace or comments still closes the
// group: `f(x++ )` is diagnosed like `f(x++)`.
fn test_postfix_operator_closes_group_skips_whitespace_and_comments() {
	for src in ['f(x++)', 'f(x++ )', 'f(x++\t)', 'f(x++ /* comment */)', 'f(x++\n)',
		'f(x++ // comment\n)', 'a[x--]', 'a[x-- ]', 'f(x++ /* outer /* inner */ end */)',
		'a[x-- /* outer /* inner */ end */]'] {
		assert postfix_operator_closes_group(src, postfix_operator_end(src)), src
	}
}

fn test_postfix_operator_value_that_closes_nothing() {
	for src in ['y := x++', 'y := x++ // )', 'y := x++\nz := 1', 'f(x++, 1)', 'y := x++ /* ) */ + 1',
		'y := x++ /* unterminated', 'y := x++ /* outer /* inner */ ) */ + 1',
		'y := x++ /* outer /* inner */ ] */ + 1'] {
		assert !postfix_operator_closes_group(src, postfix_operator_end(src)), src
	}
}
