#ifndef V_H2_SHIM_H
#define V_H2_SHIM_H

/*
 * Minimal C API around the H2 JDBC driver, implemented with the JNI
 * Invocation API in h2_shim.c. The V compiler never sees jni.h.
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct H2Conn H2Conn;

typedef struct H2Result {
	int nrows;
	int ncols;
	char **col_names; /* ncols entries */
	char **vals;      /* nrows * ncols entries; NULL marks SQL NULL */
} H2Result;

const char *h2_last_error(void);
H2Conn *h2_open(const char *url, const char *jar_path);
void h2_close(H2Conn *conn);
H2Result *h2_exec(H2Conn *conn, const char *sql, const char **params, const long long *nulls,
	int nparams);
void h2_result_free(H2Result *res);
long long h2_last_id(H2Conn *conn);

#ifdef __cplusplus
}
#endif

#endif /* V_H2_SHIM_H */
