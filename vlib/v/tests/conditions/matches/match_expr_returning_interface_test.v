interface MatchExprInterface {
	label() string
}

struct MatchExprOne {}

fn (m MatchExprOne) label() string {
	return 'one'
}

struct MatchExprTwo {}

fn (m MatchExprTwo) label() string {
	return 'two'
}

enum MatchExprVariant {
	one
	two
}

fn new_match_expr_interface(variant MatchExprVariant) MatchExprInterface {
	return match variant {
		.one { MatchExprOne{} }
		.two { MatchExprTwo{} }
	}
}

fn test_return_match_expr_as_interface() {
	assert new_match_expr_interface(.one).label() == 'one'
	assert new_match_expr_interface(.two).label() == 'two'
}
