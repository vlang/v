module oracle

// The Oracle Call Interface is provided by Oracle Instant Client. Set
// ORACLE_CLIENT_DIR to the extracted `instantclient_<version>` directory so
// that the compiler can find `sdk/lib/msvc/oci.lib` (Basic + SDK packages).
// V never parses the Oracle headers: every C type and function used here is
// declared manually in `_cdefs.c.v`.
#flag windows -L$env('ORACLE_CLIENT_DIR')/sdk/lib/msvc -loci
#flag windows -I@DIR
#include "oci_decls.h"
