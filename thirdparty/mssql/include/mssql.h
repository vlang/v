// hacking some headers in windows


#include <windows.h>

#define __TEMPUNICODE UNICODE

#ifdef UNICODE
#undef UNICODE
#endif

#include <sql.h>
#include <sqlext.h>

#define UNICODE __TEMPUNICODE
