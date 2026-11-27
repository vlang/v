#ifndef V_ORACLE_OCI_DECLS_H
#define V_ORACLE_OCI_DECLS_H

/*
 * Minimal C declarations for the OCI entry points used by db.oracle.
 * This file exists only so the C compiler sees prototypes when db.oracle is
 * imported by another module. V itself never reads or parses this header;
 * the V-side declarations live in _cdefs.c.v.
 */
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

int OCIEnvCreate(void **envp, unsigned int mode, void *ctxp, void *malocfp, void *ralocfp,
	void *mfreefp, size_t xtramem_sz, void *usrmempp);
int OCIHandleAlloc(void *parent, void *hndlpp, unsigned int htype, size_t xtramem_sz, void *usrmempp);
int OCIHandleFree(void *hndlp, unsigned int htype);
int OCILogon2(void *envp, void *errp, void **svcpp, void *user, unsigned int user_len, void *pass,
	unsigned int pass_len, void *dbname, unsigned int dbname_len, unsigned int mode);
int OCILogoff(void *svcp, void *errp);
int OCIServerAttach(void *srvp, void *errp, void *dblink, int dblink_len, unsigned int mode);
int OCIServerDetach(void *srvp, void *errp, unsigned int mode);
int OCISessionBegin(void *svcp, void *errp, void *usrp, unsigned int credt, unsigned int mode);
int OCISessionEnd(void *svcp, void *errp, void *usrp, unsigned int mode);
int OCIAttrSet(void *trgthndlp, unsigned int trghndltyp, void *attributep, unsigned int size,
	unsigned int attrtype, void *errp);
int OCIAttrGet(void *trgthndlp, unsigned int trghndltyp, void *attributep, unsigned int *sizep,
	unsigned int attrtype, void *errp);
int OCIStmtPrepare(void *stmtp, void *errp, void *stmt, unsigned int stmt_len, unsigned int language,
	unsigned int mode);
int OCIBindByPos(void *stmtp, void **bindpp, void *errp, unsigned int position, void *valuep,
	int value_sz, unsigned short dty, void *indp, void *alenp, void *rcodep,
	unsigned int maxarr_len, void *curelep, unsigned int mode);
int OCIDefineByPos(void *stmtp, void **defnpp, void *errp, unsigned int position, void *valuep,
	int value_sz, unsigned short dty, void *indp, void *rlenp, void *rcodep, unsigned int mode);
int OCIStmtExecute(void *svcp, void *stmtp, void *errp, unsigned int iters, unsigned int rowoff,
	void *snap_in, void *snap_out, unsigned int mode);
int OCIStmtFetch2(void *stmtp, void *errp, unsigned int nrows, unsigned short orientation,
	unsigned int scroll_offset, unsigned int mode);
int OCITransStart(void *svcp, void *errp, unsigned int timeout, unsigned int flags);
int OCITransCommit(void *svcp, void *errp, unsigned int flags);
int OCITransRollback(void *svcp, void *errp, unsigned int flags);
int OCIParamGet(void *hndlp, unsigned int htype, void *errp, void *parmdpp, unsigned int pos);
int OCIErrorGet(void *hndlp, unsigned int recordno, void *sqlstate, unsigned int *errcodep,
	void *bufp, unsigned int bufsiz, unsigned int htype);

#ifdef __cplusplus
}
#endif

#endif
