module forwardsource

import v.tests.c_enums.flagpositions as sourcepositions

pub enum Source {
	first = C.enum_first_opt + int(sourcepositions.Position.third)
	next
}
