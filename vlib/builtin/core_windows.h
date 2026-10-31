#ifndef V_BUILTIN_CORE_WINDOWS_H
#define V_BUILTIN_CORE_WINDOWS_H

struct ExceptionPointers;
void *v_set_unhandled_exception_filter(int (*handler)(struct ExceptionPointers *));
void *v_builtin_windows_stream(unsigned int index);
void v_builtin_windows_debugbreak(void);

#endif
