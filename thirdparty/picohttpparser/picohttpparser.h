
typedef struct phr_header phr_header_t;

#include "src/picohttpparser.h"
#include <stdint.h>

#define ADVANCE_TOKEN2(buf, tok, toklen, max_len) \
	do {\
		for (u32 i = 0; i < max_len; i++) {\
			if (buf[i] == ' ') {\
				tok = buf;\
				toklen = i++;\
				while (buf[i] == ' ') i++;\
				buf += i;\
				break;\
			}\
		}\
	} while (0)

static inline int phr_parse_request_path(const char *buf_start, size_t len, const char **method, size_t *method_len, const char **path, size_t *path_len) {
	if (len < 14) return -2;
	const char *buf = buf_start, *buf_end = buf_start + len;

	ADVANCE_TOKEN2(buf, *method, *method_len, 9);
	len -= buf-buf_start;
	ADVANCE_TOKEN2(buf, *path, *path_len, len);
	if (*method_len == 0 || *path_len == 0) return -1;

	return buf-buf_start;
}

static inline int phr_parse_request_path_pipeline(const char *buf_start, size_t len, const char **method, size_t *method_len, const char **path, size_t *path_len) {
	if (len < 14) return -2;
	const char *buf = buf_start, *buf_end = buf_start + len;

	ADVANCE_TOKEN2(buf, *method, *method_len, 9);
	len -= buf-buf_start;
	ADVANCE_TOKEN2(buf, *path, *path_len, len);
	if (*method_len == 0 || *path_len == 0) return -1;

	while (buf < buf_end) {
		++buf;
		if (*(uint32_t*)buf == 0x0a0d0a0d) {
			buf += 4;
			return (int)(buf - buf_start);
		}
	};

	return -1;
}

// date
const char* get_date();

// branchlut
const char gDigitsLut[200] = {
	'0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9',
	'1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9',
	'2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9',
	'3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9',
	'4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9',
	'5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9',
	'6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9',
	'7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9',
	'8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9',
	'9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9'
};

static inline int u64toa(char* buf, uint64_t value) {
	const char* b = buf;
	if (value < 100000000) {
		uint32_t v = (uint32_t)(value);
		if (v < 10000) {
			const uint32_t d1 = (v / 100) << 1;
			const uint32_t d2 = (v % 100) << 1;

			if (v >= 1000)
				*buf++ = gDigitsLut[d1];
			if (v >= 100)
				*buf++ = gDigitsLut[d1 + 1];
			if (v >= 10)
				*buf++ = gDigitsLut[d2];
			*buf++ = gDigitsLut[d2 + 1];
		}
		else {
			// value = bbbbcccc
			const uint32_t b = v / 10000;
			const uint32_t c = v % 10000;

			const uint32_t d1 = (b / 100) << 1;
			const uint32_t d2 = (b % 100) << 1;

			const uint32_t d3 = (c / 100) << 1;
			const uint32_t d4 = (c % 100) << 1;

			if (value >= 10000000)
				*buf++ = gDigitsLut[d1];
			if (value >= 1000000)
				*buf++ = gDigitsLut[d1 + 1];
			if (value >= 100000)
				*buf++ = gDigitsLut[d2];
			*buf++ = gDigitsLut[d2 + 1];

			*buf++ = gDigitsLut[d3];
			*buf++ = gDigitsLut[d3 + 1];
			*buf++ = gDigitsLut[d4];
			*buf++ = gDigitsLut[d4 + 1];
		}
	}
	// *buf = '\0';
	return buf - b;
}
