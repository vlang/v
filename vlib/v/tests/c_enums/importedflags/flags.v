module importedflags

import v.tests.c_enums.flagpositions { Position }

@[flag]
pub enum Flags {
	item = int(Position.third)
	next
}
