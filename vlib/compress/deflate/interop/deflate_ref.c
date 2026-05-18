#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

static int rf(const char *p, unsigned char **o, size_t *n) {
	FILE *f = fopen(p, "rb");
	if (!f) return 1;
	fseek(f, 0, SEEK_END);
	long s = ftell(f);
	fseek(f, 0, SEEK_SET);
	*o = malloc(*n = (size_t)s);
	if (!*o) {
		fclose(f);
		return 1;
	}
	if (fread(*o, 1, *n, f) != *n) {
		free(*o);
		fclose(f);
		return 1;
	}
	fclose(f);
	return 0;
}

static int wf(const char *p, const unsigned char *b, size_t n) {
	FILE *f = fopen(p, "wb");
	if (!f) return 1;
	if (fwrite(b, 1, n, f) != n) {
		fclose(f);
		return 1;
	}
	fclose(f);
	return 0;
}

int main(int argc, char **argv) {
	if (argc != 4) {
		fputs("usage: xval compress|decompress|gzip|gunzip in out\n", stderr);
		return 2;
	}
	unsigned char *in;
	size_t in_n;
	if (rf(argv[2], &in, &in_n)) {
		fputs("read error\n", stderr);
		return 1;
	}

	if (strcmp(argv[1], "compress") == 0) {
		uLongf cap = compressBound((uLong)in_n);
		unsigned char *out = malloc(cap);
		if (!out) return 1;
		if (compress2(out, &cap, in, (uLong)in_n, Z_DEFAULT_COMPRESSION) != Z_OK) {
			fputs("compress2 failed\n", stderr);
			free(in);
			free(out);
			return 1;
		}
		wf(argv[3], out, (size_t)cap);
		free(out);
	} else if (strcmp(argv[1], "decompress") == 0) {
		uLongf cap = in_n * 8 + 65536;
		unsigned char *out = malloc(cap);
		if (!out) return 1;
		if (uncompress(out, &cap, in, (uLong)in_n) != Z_OK) {
			fputs("uncompress failed\n", stderr);
			free(in);
			free(out);
			return 1;
		}
		wf(argv[3], out, (size_t)cap);
		free(out);
	} else if (strcmp(argv[1], "gzip") == 0) {
		z_stream s;
		memset(&s, 0, sizeof(s));
		if (deflateInit2(&s, Z_DEFAULT_COMPRESSION, Z_DEFLATED, 15 | 16, 8, Z_DEFAULT_STRATEGY) != Z_OK) {
			free(in);
			return 1;
		}
		uLongf cap = deflateBound(&s, (uLong)in_n) + 32;
		unsigned char *out = malloc(cap);
		if (!out) {
			deflateEnd(&s);
			free(in);
			return 1;
		}
		s.next_in = in;
		s.avail_in = (uInt)in_n;
		s.next_out = out;
		s.avail_out = (uInt)cap;
		if (deflate(&s, Z_FINISH) != Z_STREAM_END) {
			fputs("gzip deflate failed\n", stderr);
			deflateEnd(&s);
			free(in);
			free(out);
			return 1;
		}
		wf(argv[3], out, (size_t)s.total_out);
		deflateEnd(&s);
		free(out);
	} else if (strcmp(argv[1], "gunzip") == 0) {
		z_stream s;
		memset(&s, 0, sizeof(s));
		if (inflateInit2(&s, 15 | 16) != Z_OK) {
			free(in);
			return 1;
		}
		uLongf cap = in_n * 8 + 65536;
		unsigned char *out = malloc(cap);
		if (!out) {
			inflateEnd(&s);
			free(in);
			return 1;
		}
		s.next_in = in;
		s.avail_in = (uInt)in_n;
		s.next_out = out;
		s.avail_out = (uInt)cap;
		if (inflate(&s, Z_FINISH) != Z_STREAM_END) {
			fputs("gunzip inflate failed\n", stderr);
			inflateEnd(&s);
			free(in);
			free(out);
			return 1;
		}
		wf(argv[3], out, (size_t)s.total_out);
		inflateEnd(&s);
		free(out);
	}

	free(in);
	return 0;
}


