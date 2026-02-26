#ifndef ISSUE_18378_ZLIB_COMPAT_H
#define ISSUE_18378_ZLIB_COMPAT_H

typedef void *voidpf;
typedef unsigned int uInt;

typedef struct issue_18378_z_stream_s {
	int dummy;
} z_stream;

typedef voidpf (*alloc_func)(voidpf opaque, uInt items, uInt size);

#endif
