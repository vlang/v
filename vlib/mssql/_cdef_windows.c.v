module mssql

// For windows tcc compiler does not work, only msvc works.
// Some error happens in winnt.h, when using tcc. It might be caused by some tcc include headers.

// odbc32 lib comes with windows sdk and does not need to be installed separately
#flag windows -lodbc32

// #include <windows.h>
// #include <sql.h>
// #include <sqlext.h>

// in third party folder
#flag windows -I@VEXEROOT/thirdparty/mssql/include
#include <mssql.h>