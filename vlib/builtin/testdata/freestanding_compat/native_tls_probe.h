#ifndef V_NATIVE_TLS_PROBE_H
#define V_NATIVE_TLS_PROBE_H
#include <stdint.h>
int32_t v_native_libraries_prepare(void);
int32_t v_native_libraries_fetch(const char *,const char *);
int32_t v_native_libraries_formats(void);
int32_t v_native_libraries_verify(const char *,const char *);
void v_native_libraries_finish(void);
int32_t v_native_libraries_sandbox(void);
int32_t v_native_libraries_reject_certificate(const char *,const char *);
#endif
