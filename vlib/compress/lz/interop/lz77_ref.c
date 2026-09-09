#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MIN_MATCH 3
#define MAX_LITERAL 128

static const uint8_t STREAM_MAGIC[4] = {0x56, 0x4c, 0x5a, 0x31};
static const uint8_t FORMAT_LZ77 = 0;

typedef struct {
	uint8_t *data;
	size_t len;
	size_t cap;
} Buffer;

static void die(const char *msg) {
	fprintf(stderr, "%s\n", msg);
	exit(1);
}

static void buf_init(Buffer *b, size_t cap) {
	b->data = (uint8_t *)malloc(cap > 0 ? cap : 1);
	if (!b->data) {
		die("allocation failed");
	}
	b->len = 0;
	b->cap = cap > 0 ? cap : 1;
}

static void buf_push(Buffer *b, uint8_t v) {
	if (b->len >= b->cap) {
		size_t new_cap = b->cap * 2;
		uint8_t *n = (uint8_t *)realloc(b->data, new_cap);
		if (!n) {
			die("reallocation failed");
		}
		b->data = n;
		b->cap = new_cap;
	}
	b->data[b->len++] = v;
}

static void buf_append(Buffer *b, const uint8_t *src, size_t len) {
	for (size_t i = 0; i < len; i++) {
		buf_push(b, src[i]);
	}
}

static Buffer read_all(const char *path) {
	FILE *f = fopen(path, "rb");
	if (!f) {
		die("could not open input file");
	}
	if (fseek(f, 0, SEEK_END) != 0) {
		fclose(f);
		die("could not seek input file");
	}
	long sz = ftell(f);
	if (sz < 0) {
		fclose(f);
		die("could not read input file size");
	}
	if (fseek(f, 0, SEEK_SET) != 0) {
		fclose(f);
		die("could not rewind input file");
	}
	Buffer in;
	buf_init(&in, (size_t)sz + 1);
	in.len = (size_t)sz;
	if (in.len > 0 && fread(in.data, 1, in.len, f) != in.len) {
		fclose(f);
		free(in.data);
		die("could not read input file");
	}
	fclose(f);
	return in;
}

static void write_all(const char *path, const uint8_t *data, size_t len) {
	FILE *f = fopen(path, "wb");
	if (!f) {
		die("could not open output file");
	}
	if (len > 0 && fwrite(data, 1, len, f) != len) {
		fclose(f);
		die("could not write output file");
	}
	fclose(f);
}

static void write_uvarint(Buffer *out, uint64_t value) {
	uint64_t v = value;
	while (v >= 0x80) {
		buf_push(out, (uint8_t)(v & 0x7f) | 0x80);
		v >>= 7;
	}
	buf_push(out, (uint8_t)v);
}

static int read_uvarint(const uint8_t *data, size_t len, size_t *pos, uint64_t *value) {
	uint64_t out = 0;
	uint32_t shift = 0;
	while (*pos < len && shift <= 63) {
		uint8_t b = data[*pos];
		(*pos)++;
		out |= ((uint64_t)(b & 0x7f)) << shift;
		if ((b & 0x80) == 0) {
			*value = out;
			return 1;
		}
		shift += 7;
	}
	return 0;
}

static Buffer compress_lz77(const uint8_t *in, size_t in_len) {
	Buffer out;
	buf_init(&out, in_len + 32);
	buf_append(&out, STREAM_MAGIC, 4);
	buf_push(&out, FORMAT_LZ77);
	write_uvarint(&out, (uint64_t)in_len);

	for (size_t i = 0; i < in_len;) {
		size_t lit_len = in_len - i;
		if (lit_len > MAX_LITERAL) {
			lit_len = MAX_LITERAL;
		}
		buf_push(&out, (uint8_t)(lit_len - 1));
		buf_append(&out, in + i, lit_len);
		i += lit_len;
	}
	return out;
}

static Buffer decompress_lz77(const uint8_t *in, size_t in_len) {
	if (in_len < 6 || memcmp(in, STREAM_MAGIC, 4) != 0) {
		die("bad magic");
	}
	if (in[4] != FORMAT_LZ77) {
		die("format mismatch");
	}
	size_t pos = 5;
	uint64_t expected_len_u64 = 0;
	if (!read_uvarint(in, in_len, &pos, &expected_len_u64)) {
		die("bad length varint");
	}
	size_t expected_len = (size_t)expected_len_u64;

	Buffer out;
	buf_init(&out, expected_len + 16);
	while (pos < in_len) {
		uint8_t control = in[pos++];
		if ((control & 0x80) == 0) {
			size_t lit_len = (size_t)(control & 0x7f) + 1;
			if (pos + lit_len > in_len) {
				die("truncated literal");
			}
			buf_append(&out, in + pos, lit_len);
			pos += lit_len;
		} else {
			size_t match_len = (size_t)(control & 0x7f) + MIN_MATCH;
			uint64_t offset_u64 = 0;
			if (!read_uvarint(in, in_len, &pos, &offset_u64)) {
				die("bad match offset");
			}
			size_t offset = (size_t)offset_u64;
			if (offset == 0 || offset > out.len) {
				die("bad offset");
			}
			size_t base = out.len - offset;
			for (size_t k = 0; k < match_len; k++) {
				buf_push(&out, out.data[base + k]);
			}
		}
	}
	if (out.len != expected_len) {
		die("length mismatch");
	}
	return out;
}

static int64_t now_ms(void) {
	return (int64_t)((double)clock() * 1000.0 / (double)CLOCKS_PER_SEC);
}

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr,
			"usage:\n"
			"  %s bench <input.bin> <iterations>\n"
			"  %s compress <input.bin> <output.bin>\n"
			"  %s decompress <input.bin> <output.bin>\n",
			argv[0], argv[0], argv[0]);
		return 1;
	}
	if (strcmp(argv[1], "bench") == 0) {
		if (argc < 4) {
			fprintf(stderr, "usage: %s bench <input.bin> <iterations>\n", argv[0]);
			return 1;
		}
		int iterations = atoi(argv[3]);
		if (iterations <= 0) {
			fprintf(stderr, "iterations must be > 0\n");
			return 1;
		}
		Buffer input = read_all(argv[2]);
		int64_t start = now_ms();
		for (int i = 0; i < iterations; i++) {
			Buffer comp = compress_lz77(input.data, input.len);
			Buffer decomp = decompress_lz77(comp.data, comp.len);
			if (decomp.len != input.len || memcmp(decomp.data, input.data, input.len) != 0) {
				fprintf(stderr, "roundtrip mismatch\n");
				return 1;
			}
			free(comp.data);
			free(decomp.data);
		}
		int64_t elapsed = now_ms() - start;
		printf("ms=%lld\n", (long long)elapsed);
		free(input.data);
		return 0;
	}

	if (strcmp(argv[1], "compress") == 0) {
		if (argc < 4) {
			fprintf(stderr, "usage: %s compress <input.bin> <output.bin>\n", argv[0]);
			return 1;
		}
		Buffer input = read_all(argv[2]);
		Buffer comp = compress_lz77(input.data, input.len);
		write_all(argv[3], comp.data, comp.len);
		free(input.data);
		free(comp.data);
		return 0;
	}

	if (strcmp(argv[1], "decompress") == 0) {
		if (argc < 4) {
			fprintf(stderr, "usage: %s decompress <input.bin> <output.bin>\n", argv[0]);
			return 1;
		}
		Buffer input = read_all(argv[2]);
		Buffer dec = decompress_lz77(input.data, input.len);
		write_all(argv[3], dec.data, dec.len);
		free(input.data);
		free(dec.data);
		return 0;
	}

	fprintf(stderr, "unknown mode: %s\n", argv[1]);
	return 1;
}

