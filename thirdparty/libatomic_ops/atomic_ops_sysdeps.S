/*
 * Include the appropriate system-dependent assembly file, if any.
 * This is used only if the platform supports neither inline assembly
 * code, nor appropriate compiler intrinsics.
 */

#if !defined(__GNUC__) && (defined(sparc) || defined(__sparc))
#  include "atomic_ops/sysdeps/sunc/sparc.S"
#endif
