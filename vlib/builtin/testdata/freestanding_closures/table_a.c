#include "closure_freestanding.h"

void *v_test_closure_table_a(void) {
	return (void *)v_closure_static_templates;
}
