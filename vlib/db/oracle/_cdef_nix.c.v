module oracle

// On non-Windows platforms the Oracle Instant Client library is `libclntsh`
// and lives directly in the extracted client directory. As on Windows, set
// ORACLE_CLIENT_DIR so the linker can find it.
#flag !windows -L$env('ORACLE_CLIENT_DIR') -lclntsh
#flag !windows -I@DIR
#include "oci_decls.h"
