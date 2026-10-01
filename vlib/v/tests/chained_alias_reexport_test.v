// Cross-module chained aliases: module `a` re-exports `b.ID` as `a.ID`, and a
// downstream caller only needs to know about `a` (#27055).
import chained_alias_a_module as a

type ID = a.ID

fn test_chained_alias_across_modules() {
	id := ID('hello')
	assert id.str() == 'hello'
	b_id := a.ID('world')
	mid := ID(b_id)
	assert mid.str() == 'world'
}
