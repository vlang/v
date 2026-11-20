module oracle

// OCI function prototypes. Only primitive V types are used on purpose: V maps
// them to plain C types, so the Oracle headers never need to be read by V (or
// even included by the generated C). Opaque OCI handles are `voidptr`.

fn C.OCIEnvCreate(envp &voidptr, mode u32, ctxp voidptr, malocfp voidptr, ralocfp voidptr,
	mfreefp voidptr, xtramem_sz u64, usrmempp voidptr) int

fn C.OCIHandleAlloc(parent voidptr, hndlpp voidptr, htype u32, xtramem_sz u64, usrmempp voidptr) int

fn C.OCIHandleFree(hndlp voidptr, htype u32) int

fn C.OCILogon2(envp voidptr, errp voidptr, svcpp &voidptr, user voidptr, user_len u32,
	pass voidptr, pass_len u32, dbname voidptr, dbname_len u32, mode u32) int

fn C.OCILogoff(svcp voidptr, errp voidptr) int

fn C.OCIServerAttach(srvp voidptr, errp voidptr, dblink voidptr, dblink_len int, mode u32) int

fn C.OCIServerDetach(srvp voidptr, errp voidptr, mode u32) int

fn C.OCISessionBegin(svcp voidptr, errp voidptr, usrp voidptr, credt u32, mode u32) int

fn C.OCISessionEnd(svcp voidptr, errp voidptr, usrp voidptr, mode u32) int

fn C.OCIAttrSet(trgthndlp voidptr, trghndltyp u32, attributep voidptr, size u32, attrtype u32,
	errp voidptr) int

fn C.OCIAttrGet(trgthndlp voidptr, trghndltyp u32, attributep voidptr, sizep &u32, attrtype u32,
	errp voidptr) int

fn C.OCIStmtPrepare(stmtp voidptr, errp voidptr, stmt voidptr, stmt_len u32, language u32,
	mode u32) int

fn C.OCIBindByPos(stmtp voidptr, bindpp &voidptr, errp voidptr, position u32, valuep voidptr,
	value_sz int, dty u16, indp voidptr, alenp voidptr, rcodep voidptr, maxarr_len u32,
	curelep voidptr, mode u32) int

fn C.OCIDefineByPos(stmtp voidptr, defnpp &voidptr, errp voidptr, position u32, valuep voidptr,
	value_sz int, dty u16, indp voidptr, rlenp voidptr, rcodep voidptr, mode u32) int

fn C.OCIStmtExecute(svcp voidptr, stmtp voidptr, errp voidptr, iters u32, rowoff u32,
	snap_in voidptr, snap_out voidptr, mode u32) int

fn C.OCIStmtFetch2(stmtp voidptr, errp voidptr, nrows u32, orientation u16, scroll_offset u32,
	mode u32) int

fn C.OCITransStart(svcp voidptr, errp voidptr, timeout u32, flags u32) int

fn C.OCITransCommit(svcp voidptr, errp voidptr, flags u32) int

fn C.OCITransRollback(svcp voidptr, errp voidptr, flags u32) int

fn C.OCIParamGet(hndlp voidptr, htype u32, errp voidptr, parmdpp voidptr, pos u32) int

fn C.OCIErrorGet(hndlp voidptr, recordno u32, sqlstate voidptr, errcodep &u32, bufp voidptr,
	bufsiz u32, htype u32) int
