import v.tests.c_enums.forwardsource

#include "@VMODROOT/vlib/v/tests/c_enums/lowercase_c_enum.h"

enum ImportedCopy {
	first = int(forwardsource.Source.next)
}

enum ForwardTransitive {
	first = int(ForwardCopy.next)
	next
}

enum ForwardCopy {
	first = int(ForwardSource.first)
	next
}

enum ForwardTransitiveBacked as u64 {
	first = u64(ForwardBackedCopy.next)
	next
}

enum ForwardBackedCopy as u64 {
	first = u64(ForwardBackedSource.first)
	next
}

enum ForwardBackedSource as u64 {
	first = C.enum_first_opt
	next
}

enum NarrowCopy {
	first = int(NarrowSource.first)
}

enum NarrowSource as u8 {
	first = C.enum_first_opt + 254
}

enum ForwardSource {
	first = C.enum_first_opt
	next
}

fn test_forward_c_derived_enum_copies() {
	assert int(ForwardCopy.first) == 3
	assert int(ForwardCopy.next) == 4
	assert int(ForwardTransitive.first) == 4
	assert int(ForwardTransitive.next) == 5
	assert u64(ForwardBackedCopy.first) == 3
	assert u64(ForwardBackedCopy.next) == 4
	assert u64(ForwardTransitiveBacked.first) == 4
	assert u64(ForwardTransitiveBacked.next) == 5
	assert int(NarrowCopy.first) == 1
	assert int(ImportedCopy.first) == 7
}
