type AliasMatchSum = int | string
type AliasMatchSumAlias = AliasMatchSum

fn test_match_on_sumtype_alias_variants() {
	a := AliasMatchSumAlias(10)
	result := match a {
		int { a.str() }
		string { a }
	}
	assert result == '10'
}

type AliasMatchParseRes = AliasMatchResult[[]AliasMatchToken, AliasMatchParseErr]

struct AliasMatchToken {}

struct AliasMatchParseErr {}

type AliasMatchOpt[T] = AliasMatchNone[T] | AliasMatchSome[T]

struct AliasMatchNone[T] {}

struct AliasMatchSome[T] {
	value T
}

type AliasMatchResult[T, U] = AliasMatchErr[U] | AliasMatchOk[T]

struct AliasMatchOk[T] {
	value T
}

struct AliasMatchErr[U] {
	value U
}

fn test_match_on_sumtype_alias_parent_type_regression() {
	r := AliasMatchOpt[AliasMatchParseRes](AliasMatchNone[AliasMatchParseRes]{})
	match r {
		AliasMatchSome[AliasMatchParseRes] {
			match r.value {
				AliasMatchResult[[]AliasMatchToken, AliasMatchParseErr] {}
				else {}
			}
		}
		AliasMatchNone[AliasMatchParseRes] {
			assert true
		}
	}
}
