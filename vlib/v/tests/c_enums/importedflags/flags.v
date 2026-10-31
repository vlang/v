module importedflags

import v.tests.c_enums.flagpositions { Position }

// This consumer alias must not affect the imported enum's initializer.
type PositionIndex = string

@[flag]
pub enum Flags {
	item = int(Position.third)
	next
}
