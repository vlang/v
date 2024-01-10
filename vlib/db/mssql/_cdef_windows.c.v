module mssql

// mssql module does not support tcc on windows

// odbc32 lib comes with windows sdk and does not need to be installed separately.
// v builder for msvc can resolve the sdk includes search path, so no need to repeat here.
#flag windows -lodbc32

// Special handling of sql headers on windows.
// Source is in v third party folder.
#flag windows -I@VEXEROOT/thirdparty/mssql/include
#include <mssql.h>
