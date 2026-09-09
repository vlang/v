import sync.stdatomic as _

#include "@VEXEROOT/vlib/sync/stdatomic/stdatomic_include_after_compat.h"

fn C.v_test_atomic_after_compat_include() usize

fn test_system_stdatomic_can_be_included_after_v_compat_header() {
	assert C.v_test_atomic_after_compat_include() == 7
}
