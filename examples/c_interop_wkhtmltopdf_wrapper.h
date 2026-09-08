#ifndef V_C_INTEROP_WKHTMLTOPDF_WRAPPER_H
#define V_C_INTEROP_WKHTMLTOPDF_WRAPPER_H

#include <wkhtmltox/pdf.h>

static inline char *v_example_wkhtmltopdf_version(void) {
	return (char *)wkhtmltopdf_version();
}

#endif
