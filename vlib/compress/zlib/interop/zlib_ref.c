#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

static int rf(const char* p, unsigned char** o, size_t* n)
{
    FILE* f = fopen(p, "rb");
    if (!f) return 1;
    if (fseek(f, 0, SEEK_END) != 0)
    {
        fclose(f);
        return 1;
    }
    long s = ftell(f);
    if (s < 0)
    {
        fclose(f);
        return 1;
    }
    if (fseek(f, 0, SEEK_SET) != 0)
    {
        fclose(f);
        return 1;
    }
    *n = (size_t)s;
    *o = *n ? (unsigned char*)malloc(*n) : NULL;
    if (*n && !*o)
    {
        fclose(f);
        return 1;
    }
    if (*n && fread(*o, 1, *n, f) != *n)
    {
        free(*o);
        fclose(f);
        return 1;
    }
    fclose(f);
    return 0;
}

static int wf(const char* p, const unsigned char* b, size_t n)
{
    FILE* f = fopen(p, "wb");
    if (!f) return 1;
    if (n && fwrite(b, 1, n, f) != n)
    {
        fclose(f);
        return 1;
    }
    fclose(f);
    return 0;
}

int main(int argc, char** argv)
{
    static const unsigned char dummy = 0;
    if (argc != 4)
    {
        fputs("usage: zlib_ref compress|decompress in out\n", stderr);
        return 2;
    }
    unsigned char* in = NULL;
    size_t in_n = 0;
    if (rf(argv[2], &in, &in_n))
    {
        fputs("read error\n", stderr);
        return 1;
    }
    const unsigned char* in_ptr = in_n ? in : &dummy;
    if (strcmp(argv[1], "compress") == 0)
    {
        uLongf out_n = compressBound((uLong)in_n);
        unsigned char* out = (unsigned char*)malloc(out_n ? out_n : 1);
        if (!out)
        {
            free(in);
            return 1;
        }
        if (compress2(out, &out_n, in_ptr, (uLong)in_n, Z_DEFAULT_COMPRESSION) != Z_OK)
        {
            fputs("compress2 failed\n", stderr);
            free(in);
            free(out);
            return 1;
        }
        if (wf(argv[3], out, (size_t)out_n))
        {
            fputs("write error\n", stderr);
            free(in);
            free(out);
            return 1;
        }
        free(out);
    }
    else if (strcmp(argv[1], "decompress") == 0)
    {
        uLongf out_n = in_n * 8 + 64;
        if (out_n < 256) out_n = 256;
        unsigned char* out = NULL;
        int rc = Z_BUF_ERROR;
        while (rc == Z_BUF_ERROR)
        {
            unsigned char* next = (unsigned char*)realloc(out, out_n);
            if (!next)
            {
                free(in);
                free(out);
                return 1;
            }
            out = next;
            uLongf cap = out_n;
            rc = uncompress(out, &cap, in_ptr, (uLong)in_n);
            if (rc == Z_OK)
            {
                out_n = cap;
                break;
            }
            if (rc == Z_BUF_ERROR)
            {
                out_n *= 2;
                if (out_n < 256) out_n = 256;
            }
        }
        if (rc != Z_OK)
        {
            fputs("uncompress failed\n", stderr);
            free(in);
            free(out);
            return 1;
        }
        if (wf(argv[3], out, (size_t)out_n))
        {
            fputs("write error\n", stderr);
            free(in);
            free(out);
            return 1;
        }
        free(out);
    }
    else
    {
        fputs("unknown mode\n", stderr);
        free(in);
        return 2;
    }
    free(in);
    return 0;
}
