#include "closure_freestanding.h"

void *v_test_closure_table_b(void) {
	return (void *)v_closure_static_templates;
}
