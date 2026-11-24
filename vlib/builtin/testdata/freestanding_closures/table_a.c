#include "closure_freestanding.h"

void *v_test_closure_table_a(void) {
	return v_closure_map_static_templates(16384);
}
