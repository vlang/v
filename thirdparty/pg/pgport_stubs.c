/*
 * Minimal libpgport/libpgcommon stubs for V's `db.pg` cross compilation to
 * Linux. The bundled `linuxroot` sysroot ships a static `libpq.a` but no
 * `libpgcommon.a` / `libpgport.a`, so a number of symbols referenced by
 * libpq stay unresolved at link time. The implementations here cover only
 * the entry points actually referenced by the libpq.a frontend (verified
 * via `ld.lld --error-limit=0`), forwarding to libc / OpenSSL equivalents.
 * They are NOT a full replacement for libpgport / libpgcommon: SASLprep
 * is a no-op, encoding tables only know the name list, etc. Enough to
 * connect to a Postgres server using SCRAM-SHA-256 and exchange queries
 * in SQL_ASCII / UTF8.
 *
 * Linked only via `$if cross_compile ? && linux` in vlib/db/pg/pg.c.v.
 * The linuxroot sysroot lacks some POSIX headers (e.g. pwd.h), so the few
 * libc entry points used here are declared inline instead of pulled from
 * system headers.
 */

#define _GNU_SOURCE
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>

/* Forward declarations to avoid relying on headers the linuxroot sysroot
 * doesn't ship (pwd.h, full netdb.h getaddrinfo/getnameinfo prototypes,
 * sys/un.h ucred). */
struct passwd;
struct addrinfo;
struct sockaddr;

extern int getpwuid_r(unsigned int uid, struct passwd *pwd, char *buf,
                      size_t buflen, struct passwd **result);
extern int getaddrinfo(const char *node, const char *service,
                       const struct addrinfo *hints, struct addrinfo **res);
extern void freeaddrinfo(struct addrinfo *res);
extern int getnameinfo(const struct sockaddr *sa, unsigned int salen, char *host,
                       unsigned int hostlen, char *serv, unsigned int servlen,
                       int flags);
extern const char *inet_ntop(int af, const void *src, char *dst, unsigned int size);

/* OpenSSL — already linked via -lssl -lcrypto. We declare what we need so we
 * don't depend on <openssl/*.h> being on the include search path. */
typedef struct evp_md_st EVP_MD;
typedef struct hmac_ctx_st HMAC_CTX;
typedef struct engine_st ENGINE;
extern const EVP_MD *EVP_sha224(void);
extern const EVP_MD *EVP_sha256(void);
extern const EVP_MD *EVP_sha384(void);
extern const EVP_MD *EVP_sha512(void);
extern const EVP_MD *EVP_md5(void);
extern const EVP_MD *EVP_sha1(void);
extern HMAC_CTX *HMAC_CTX_new(void);
extern void HMAC_CTX_free(HMAC_CTX *ctx);
extern int HMAC_Init_ex(HMAC_CTX *ctx, const void *key, int len,
                        const EVP_MD *md, ENGINE *impl);
extern int HMAC_Update(HMAC_CTX *ctx, const unsigned char *data, size_t len);
extern int HMAC_Final(HMAC_CTX *ctx, unsigned char *md, unsigned int *len);
extern int RAND_bytes(unsigned char *buf, int num);

size_t strlcpy(char *dst, const char *src, size_t siz) {
	const char *s = src;
	size_t n = siz;
	if (n != 0) {
		while (--n != 0) {
			if ((*dst++ = *s++) == '\0') {
				break;
			}
		}
	}
	if (n == 0) {
		if (siz != 0) {
			*dst = '\0';
		}
		while (*s++) {
		}
	}
	return (size_t)(s - src - 1);
}

int pg_vsnprintf(char *str, size_t count, const char *fmt, va_list args) {
	return vsnprintf(str, count, fmt, args);
}

int pg_snprintf(char *str, size_t count, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	int n = vsnprintf(str, count, fmt, ap);
	va_end(ap);
	return n;
}

int pg_sprintf(char *str, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	int n = vsprintf(str, fmt, ap);
	va_end(ap);
	return n;
}

int pg_vfprintf(FILE *stream, const char *fmt, va_list args) {
	return vfprintf(stream, fmt, args);
}

int pg_fprintf(FILE *stream, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	int n = vfprintf(stream, fmt, ap);
	va_end(ap);
	return n;
}

int pg_printf(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	int n = vprintf(fmt, ap);
	va_end(ap);
	return n;
}

int pg_strcasecmp(const char *s1, const char *s2) {
	return strcasecmp(s1, s2);
}

int pg_strncasecmp(const char *s1, const char *s2, size_t n) {
	return strncasecmp(s1, s2, n);
}

unsigned char pg_tolower(unsigned char ch) {
	if (ch >= 'A' && ch <= 'Z') {
		return ch + ('a' - 'A');
	}
	return ch;
}

unsigned char pg_toupper(unsigned char ch) {
	if (ch >= 'a' && ch <= 'z') {
		return ch - ('a' - 'A');
	}
	return ch;
}

int pg_strip_crlf(char *str) {
	int len = (int)strlen(str);
	while (len > 0 && (str[len - 1] == '\n' || str[len - 1] == '\r')) {
		str[--len] = '\0';
	}
	return len;
}

char *pg_strerror_r(int errnum, char *buf, size_t buflen) {
	const char *s = strerror(errnum);
	if (s == NULL) {
		snprintf(buf, buflen, "errno %d", errnum);
	} else {
		strncpy(buf, s, buflen);
		if (buflen > 0) {
			buf[buflen - 1] = '\0';
		}
	}
	return buf;
}

int pqGetpwuid(unsigned int uid, struct passwd *resbuf, char *buf, size_t buflen,
               struct passwd **result) {
	return getpwuid_r(uid, resbuf, buf, buflen, result);
}

int pg_getaddrinfo_all(const char *hostname, const char *servname,
                       const struct addrinfo *hint, struct addrinfo **result) {
	return getaddrinfo(hostname, servname, hint, result);
}

void pg_freeaddrinfo_all(int hint_ai_family, struct addrinfo *ai) {
	(void)hint_ai_family;
	if (ai != NULL) {
		freeaddrinfo(ai);
	}
}

int pg_getnameinfo_all(const void *addr, int salen, char *node, size_t nodelen,
                       char *service, size_t servicelen, int flags) {
	return getnameinfo((const struct sockaddr *)addr, (unsigned int)salen, node,
	                   (unsigned int)nodelen, service, (unsigned int)servicelen,
	                   flags);
}

int pg_set_noblock(int sock) {
	int flags = fcntl(sock, F_GETFL);
	if (flags < 0) {
		return 0;
	}
	if (fcntl(sock, F_SETFL, flags | O_NONBLOCK) < 0) {
		return 0;
	}
	return 1;
}

/* Linux-only: getpeereid via SO_PEERCRED. */
struct pg_ucred_compat { unsigned int pid; unsigned int uid; unsigned int gid; };
#ifndef SO_PEERCRED
#define SO_PEERCRED 17
#endif

int getpeereid(int sock, unsigned int *euid, unsigned int *egid) {
	struct pg_ucred_compat cred;
	unsigned int len = sizeof(cred);
	if (getsockopt(sock, SOL_SOCKET, SO_PEERCRED, &cred, &len) < 0) {
		return -1;
	}
	if (euid != NULL) {
		*euid = cred.uid;
	}
	if (egid != NULL) {
		*egid = cred.gid;
	}
	return 0;
}

const char *pg_inet_net_ntop(int af, const void *src, int bits, char *dst,
                             size_t size) {
	(void)bits;
	return inet_ntop(af, src, dst, (unsigned int)size);
}

/* SASLprep: a real implementation normalizes Unicode per RFC 4013. The
 * frontend uses it on passwords before SCRAM hashing; if we just pass the
 * raw bytes through, ASCII-only passwords still hash to the same value as
 * any compliant client would produce. Non-ASCII passwords may fail to
 * authenticate. PG_SASLPREP_SUCCESS == 0 in PostgreSQL. */
int pg_saslprep(const char *input, char **output) {
	size_t n = strlen(input);
	char *copy = (char *)malloc(n + 1);
	if (copy == NULL) {
		return -1; /* PG_SASLPREP_OOM */
	}
	memcpy(copy, input, n + 1);
	*output = copy;
	return 0;
}

int pg_strong_random(void *buf, size_t len) {
	if (RAND_bytes((unsigned char *)buf, (int)len) == 1) {
		return 1;
	}
	return 0;
}

/* Base64 codec: standard alphabet, identical layout to PostgreSQL's
 * src/common/base64.c so libpq's helpers (which expect specific overflow
 * semantics) behave the same. */
static const char b64_alphabet[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static const signed char b64_lookup[256] = {
    [0 ... 255] = -1,
    ['A'] = 0,  ['B'] = 1,  ['C'] = 2,  ['D'] = 3,  ['E'] = 4,  ['F'] = 5,
    ['G'] = 6,  ['H'] = 7,  ['I'] = 8,  ['J'] = 9,  ['K'] = 10, ['L'] = 11,
    ['M'] = 12, ['N'] = 13, ['O'] = 14, ['P'] = 15, ['Q'] = 16, ['R'] = 17,
    ['S'] = 18, ['T'] = 19, ['U'] = 20, ['V'] = 21, ['W'] = 22, ['X'] = 23,
    ['Y'] = 24, ['Z'] = 25, ['a'] = 26, ['b'] = 27, ['c'] = 28, ['d'] = 29,
    ['e'] = 30, ['f'] = 31, ['g'] = 32, ['h'] = 33, ['i'] = 34, ['j'] = 35,
    ['k'] = 36, ['l'] = 37, ['m'] = 38, ['n'] = 39, ['o'] = 40, ['p'] = 41,
    ['q'] = 42, ['r'] = 43, ['s'] = 44, ['t'] = 45, ['u'] = 46, ['v'] = 47,
    ['w'] = 48, ['x'] = 49, ['y'] = 50, ['z'] = 51, ['0'] = 52, ['1'] = 53,
    ['2'] = 54, ['3'] = 55, ['4'] = 56, ['5'] = 57, ['6'] = 58, ['7'] = 59,
    ['8'] = 60, ['9'] = 61, ['+'] = 62, ['/'] = 63,
};

int pg_b64_enc_len(int srclen) {
	return ((srclen + 2) / 3) * 4;
}

int pg_b64_dec_len(int srclen) {
	return (srclen / 4) * 3;
}

int pg_b64_encode(const char *src, int len, char *dst, int dstlen) {
	int i = 0, j = 0;
	while (i < len) {
		unsigned int b0 = (unsigned char)src[i++];
		unsigned int b1 = (i < len) ? (unsigned char)src[i++] : 0;
		unsigned int b2 = (i < len) ? (unsigned char)src[i++] : 0;
		int rem = (i > len) ? (i - len) : 0;
		if (j + 4 > dstlen) {
			return -1;
		}
		dst[j++] = b64_alphabet[b0 >> 2];
		dst[j++] = b64_alphabet[((b0 & 0x03) << 4) | (b1 >> 4)];
		dst[j++] = (rem >= 2) ? '=' : b64_alphabet[((b1 & 0x0f) << 2) | (b2 >> 6)];
		dst[j++] = (rem >= 1) ? '=' : b64_alphabet[b2 & 0x3f];
	}
	return j;
}

int pg_b64_decode(const char *src, int len, char *dst, int dstlen) {
	int i = 0, j = 0;
	while (i < len) {
		while (i < len && (src[i] == '\n' || src[i] == '\r' || src[i] == ' ' ||
		                   src[i] == '\t')) {
			i++;
		}
		if (i >= len) {
			break;
		}
		signed char c0 = b64_lookup[(unsigned char)src[i++]];
		if (c0 < 0) {
			return -1;
		}
		while (i < len && (src[i] == '\n' || src[i] == '\r' || src[i] == ' ' ||
		                   src[i] == '\t')) {
			i++;
		}
		if (i >= len) {
			return -1;
		}
		signed char c1 = b64_lookup[(unsigned char)src[i++]];
		if (c1 < 0) {
			return -1;
		}
		while (i < len && (src[i] == '\n' || src[i] == '\r' || src[i] == ' ' ||
		                   src[i] == '\t')) {
			i++;
		}
		signed char c2 = -1, c3 = -1;
		char raw2 = 0, raw3 = 0;
		if (i < len) {
			raw2 = src[i++];
			if (raw2 != '=') {
				c2 = b64_lookup[(unsigned char)raw2];
				if (c2 < 0) {
					return -1;
				}
			}
		}
		while (i < len && (src[i] == '\n' || src[i] == '\r' || src[i] == ' ' ||
		                   src[i] == '\t')) {
			i++;
		}
		if (i < len) {
			raw3 = src[i++];
			if (raw3 != '=') {
				c3 = b64_lookup[(unsigned char)raw3];
				if (c3 < 0) {
					return -1;
				}
			}
		}
		if (j >= dstlen) {
			return -1;
		}
		dst[j++] = (char)((c0 << 2) | ((c1 & 0x30) >> 4));
		if (raw2 == '=') {
			break;
		}
		if (j >= dstlen) {
			return -1;
		}
		dst[j++] = (char)(((c1 & 0x0f) << 4) | ((c2 & 0x3c) >> 2));
		if (raw3 == '=') {
			break;
		}
		if (j >= dstlen) {
			return -1;
		}
		dst[j++] = (char)(((c2 & 0x03) << 6) | c3);
	}
	return j;
}

/* pg_hmac: thin wrapper over OpenSSL HMAC_CTX. PG's pg_cryptohash_type:
 *   0 PG_MD5, 1 PG_SHA1, 2 PG_SHA224, 3 PG_SHA256, 4 PG_SHA384, 5 PG_SHA512 */
struct pg_hmac_ctx {
	HMAC_CTX *ctx;
	const EVP_MD *md;
	int type;
	int error;
};

struct pg_hmac_ctx *pg_hmac_create(int type) {
	const EVP_MD *md = NULL;
	switch (type) {
	case 0: md = EVP_md5(); break;
	case 1: md = EVP_sha1(); break;
	case 2: md = EVP_sha224(); break;
	case 3: md = EVP_sha256(); break;
	case 4: md = EVP_sha384(); break;
	case 5: md = EVP_sha512(); break;
	default: return NULL;
	}
	if (md == NULL) {
		return NULL;
	}
	struct pg_hmac_ctx *c = (struct pg_hmac_ctx *)malloc(sizeof(*c));
	if (c == NULL) {
		return NULL;
	}
	c->md = md;
	c->type = type;
	c->error = 0;
	c->ctx = HMAC_CTX_new();
	if (c->ctx == NULL) {
		free(c);
		return NULL;
	}
	return c;
}

int pg_hmac_init(struct pg_hmac_ctx *ctx, const unsigned char *key, size_t len) {
	if (ctx == NULL) {
		return -1;
	}
	if (HMAC_Init_ex(ctx->ctx, key, (int)len, ctx->md, NULL) != 1) {
		ctx->error = 1;
		return -1;
	}
	return 0;
}

int pg_hmac_update(struct pg_hmac_ctx *ctx, const unsigned char *data, size_t len) {
	if (ctx == NULL) {
		return -1;
	}
	if (HMAC_Update(ctx->ctx, data, len) != 1) {
		ctx->error = 1;
		return -1;
	}
	return 0;
}

int pg_hmac_final(struct pg_hmac_ctx *ctx, unsigned char *dest, size_t len) {
	if (ctx == NULL) {
		return -1;
	}
	unsigned int outlen = (unsigned int)len;
	if (HMAC_Final(ctx->ctx, dest, &outlen) != 1) {
		ctx->error = 1;
		return -1;
	}
	return 0;
}

void pg_hmac_free(struct pg_hmac_ctx *ctx) {
	if (ctx == NULL) {
		return;
	}
	if (ctx->ctx != NULL) {
		HMAC_CTX_free(ctx->ctx);
	}
	free(ctx);
}

const char *pg_hmac_error(struct pg_hmac_ctx *ctx) {
	if (ctx == NULL) {
		return "out of memory";
	}
	if (ctx->error) {
		return "HMAC error";
	}
	return "";
}

/* SCRAM-SHA-256 helpers. Signatures match PG 14 src/common/scram-common.c.
 * SCRAM_KEY_LEN is 32 (SHA-256 output). */
extern int PKCS5_PBKDF2_HMAC(const char *pass, int passlen,
                              const unsigned char *salt, int saltlen, int iter,
                              const EVP_MD *digest, int keylen,
                              unsigned char *out);

static int scram_hmac_one(const unsigned char *key, const char *msg,
                          unsigned char *result, const char **errstr) {
	struct pg_hmac_ctx *ctx = pg_hmac_create(3 /* PG_SHA256 */);
	if (ctx == NULL) {
		if (errstr != NULL) {
			*errstr = "out of memory";
		}
		return -1;
	}
	if (pg_hmac_init(ctx, key, 32) < 0 ||
	    pg_hmac_update(ctx, (const unsigned char *)msg, strlen(msg)) < 0 ||
	    pg_hmac_final(ctx, result, 32) < 0) {
		if (errstr != NULL) {
			*errstr = pg_hmac_error(ctx);
		}
		pg_hmac_free(ctx);
		return -1;
	}
	pg_hmac_free(ctx);
	return 0;
}

int scram_ServerKey(const unsigned char *salted_password, unsigned char *result,
                    const char **errstr) {
	return scram_hmac_one(salted_password, "Server Key", result, errstr);
}

int scram_ClientKey(const unsigned char *salted_password, unsigned char *result,
                    const char **errstr) {
	return scram_hmac_one(salted_password, "Client Key", result, errstr);
}

/* SHA-256 of input → result (32 bytes). */
extern unsigned char *SHA256(const unsigned char *d, size_t n, unsigned char *md);
int scram_H(const unsigned char *input, int len, unsigned char *result,
            const char **errstr) {
	if (SHA256(input, (size_t)len, result) == NULL) {
		if (errstr != NULL) {
			*errstr = "SHA256 failed";
		}
		return -1;
	}
	return 0;
}

/* PBKDF2-HMAC-SHA256(password, salt, iterations) → 32-byte salted password. */
int scram_SaltedPassword(const char *password, const char *salt, int saltlen,
                         int iterations, unsigned char *result,
                         const char **errstr) {
	if (PKCS5_PBKDF2_HMAC(password, (int)strlen(password),
	                      (const unsigned char *)salt, saltlen, iterations,
	                      EVP_sha256(), 32, result) != 1) {
		if (errstr != NULL) {
			*errstr = "PBKDF2 failed";
		}
		return -1;
	}
	return 0;
}

/* scram_build_secret is server-side; the archive references it because some
 * objects in libpq.a were compiled with both frontend and backend sections
 * sharing scram-common.c. Provide a stub that returns NULL — the frontend
 * code path that actually calls this would be unreachable. */
char *scram_build_secret(const char *salt, int saltlen, int iterations,
                         const char *password, const char **errstr) {
	(void)salt;
	(void)saltlen;
	(void)iterations;
	(void)password;
	if (errstr != NULL) {
		*errstr = "scram_build_secret not implemented in frontend stubs";
	}
	return NULL;
}

/* pg_md5_encrypt: legacy "md5"-prefixed MD5(password || username) hash. */
extern unsigned char *MD5(const unsigned char *d, size_t n, unsigned char *md);
int pg_md5_encrypt(const char *passwd, const char *salt, size_t salt_len,
                   char *buf) {
	static const char hex[] = "0123456789abcdef";
	size_t passwd_len = strlen(passwd);
	unsigned char digest[16];
	unsigned char *tmp = (unsigned char *)malloc(passwd_len + salt_len);
	if (tmp == NULL) {
		return 0;
	}
	memcpy(tmp, passwd, passwd_len);
	memcpy(tmp + passwd_len, salt, salt_len);
	if (MD5(tmp, passwd_len + salt_len, digest) == NULL) {
		free(tmp);
		return 0;
	}
	free(tmp);
	buf[0] = 'm';
	buf[1] = 'd';
	buf[2] = '5';
	for (int i = 0; i < 16; i++) {
		buf[3 + 2 * i] = hex[digest[i] >> 4];
		buf[3 + 2 * i + 1] = hex[digest[i] & 0x0f];
	}
	buf[35] = '\0';
	return 1;
}

/* Encoding helpers: the linuxroot libpq.a is built without libpgcommon, so the
 * client-side encoding tables are absent. Returning sensible defaults keeps
 * basic libpq usage (connect, query in SQL_ASCII / UTF8) working. */

int pg_get_encoding_from_locale(const char *ctype, int write_message) {
	(void)ctype;
	(void)write_message;
	return -1;
}

static const char *const pg_encoding_names[] = {
	"SQL_ASCII", "EUC_JP", "EUC_CN", "EUC_KR", "EUC_TW", "EUC_JIS_2004",
	"UTF8", "MULE_INTERNAL", "LATIN1", "LATIN2", "LATIN3", "LATIN4", "LATIN5",
	"LATIN6", "LATIN7", "LATIN8", "LATIN9", "LATIN10", "WIN1256", "WIN1258",
	"WIN866", "WIN874", "KOI8R", "WIN1251", "WIN1252", "ISO_8859_5", "ISO_8859_6",
	"ISO_8859_7", "ISO_8859_8", "WIN1250", "WIN1253", "WIN1254", "WIN1255",
	"WIN1257", "KOI8U", "SJIS", "BIG5", "GBK", "UHC", "GB18030", "JOHAB",
	"SHIFT_JIS_2004",
};

const char *pg_encoding_to_char(int encoding) {
	const int n = (int)(sizeof(pg_encoding_names) / sizeof(pg_encoding_names[0]));
	if (encoding < 0 || encoding >= n) {
		return "SQL_ASCII";
	}
	return pg_encoding_names[encoding];
}

int pg_char_to_encoding(const char *name) {
	const int n = (int)(sizeof(pg_encoding_names) / sizeof(pg_encoding_names[0]));
	if (name == NULL) {
		return -1;
	}
	for (int i = 0; i < n; i++) {
		if (strcasecmp(name, pg_encoding_names[i]) == 0) {
			return i;
		}
	}
	return -1;
}

int pg_valid_server_encoding_id(int encoding) {
	return encoding == 0 || encoding == 6;
}

/* Multibyte helpers. Treat SQL_ASCII (0) and UTF8 (6) properly; for other
 * encodings, fall back to UTF8-shaped behaviour. Good enough for libpq's
 * frontend encoding probes, but not a substitute for libpgcommon's tables. */
int pg_encoding_max_length(int encoding) {
	if (encoding == 0 /* SQL_ASCII */) {
		return 1;
	}
	return 4; /* UTF-8 worst case */
}

int pg_encoding_mblen(int encoding, const char *mbstr) {
	unsigned char c = (unsigned char)*mbstr;
	if (encoding == 0 /* SQL_ASCII */) {
		return 1;
	}
	if (c < 0x80) {
		return 1;
	}
	if ((c & 0xe0) == 0xc0) {
		return 2;
	}
	if ((c & 0xf0) == 0xe0) {
		return 3;
	}
	if ((c & 0xf8) == 0xf0) {
		return 4;
	}
	return 1;
}

int pg_encoding_dsplen(int encoding, const char *mbstr) {
	(void)encoding;
	unsigned char c = (unsigned char)*mbstr;
	if (c == 0) {
		return 0;
	}
	if (c < 0x20 || c == 0x7f) {
		return -1; /* control char: caller decides */
	}
	return 1;
}

/* libpgcommon link canary: signals that libpq was linked with the matching
 * libpgcommon flavour. We only have the frontend, so always report frontend. */
int pg_link_canary_is_frontend(void) {
	return 1;
}
