struct CastUnit {
	v int
}

struct CastSum {
	a int
}

struct CastProduct {
	b int
}

// Both sums list `CastSum` and `CastProduct`.
type CastMath = CastProduct | CastSum

type CastNumeric = CastProduct | CastSum | CastUnit

fn (m CastMath) describe(tag string) string {
	return match m {
		CastSum { '${tag}:sum ${m.a}' }
		CastProduct { '${tag}:prod ${m.b}' }
	}
}

fn (u CastUnit) describe(tag string) string {
	return '${tag}:unit ${u.v}'
}

fn show_math(m CastMath) string {
	return m.describe('show')
}

fn (n CastNumeric) describe(tag string) string {
	return match n {
		CastUnit {
			n.describe(tag)
		}
		CastSum, CastProduct {
			m := CastMath(n)
			m.describe(tag) + ' ' + show_math(CastMath(n))
		}
	}
}

fn single_math(n CastNumeric) string {
	if n is CastSum {
		return CastMath(n).describe('single')
	}
	return '-'
}

fn test_cast_of_a_smartcast_variant_to_another_sum_that_lists_it() {
	assert CastNumeric(CastUnit{1}).describe('x') == 'x:unit 1'
	assert CastNumeric(CastSum{2}).describe('x') == 'x:sum 2 show:sum 2'
	assert CastNumeric(CastProduct{3}).describe('x') == 'x:prod 3 show:prod 3'
	assert single_math(CastNumeric(CastSum{2})) == 'single:sum 2'
	assert single_math(CastNumeric(CastProduct{3})) == '-'
}
