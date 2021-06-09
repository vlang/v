// Hacking some headers in windows.
// sql headers using UNICODE to change function signatures.
// Currently Linux bindings do not use unicode SQL C bindings,
// So we turn off the UNICODE to make it compile on windows.
// For future Unicode support, please raise a issue.

#include <windows.h>
#include <sal.h>

#ifdef UNICODE
// Turn off unicode macro and turn back on, so it only affects sql headers
#undef UNICODE
#include <sql.h>
#include <sqlext.h>
#define UNICODE

#else
#include <sql.h>
#include <sqlext.h>
#endif