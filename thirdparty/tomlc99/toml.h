/*
MIT License

Copyright (c) 2017 - 2019 CK Tan
https://github.com/cktan/tomlc99

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef TOML_H
#define TOML_H


#include <stdio.h>
#include <stdint.h>


#ifdef __cplusplus
#define TOML_EXTERN extern "C"
#else
#define TOML_EXTERN extern
#endif

typedef struct toml_table_t toml_table_t;
typedef struct toml_array_t toml_array_t;

/* Parse a file. Return a table on success, or 0 otherwise. 
 * Caller must toml_free(the-return-value) after use.
 */
TOML_EXTERN toml_table_t* toml_parse_file(FILE* fp, 
                                          char* errbuf,
                                          int errbufsz);

/* Parse a string containing the full config. 
 * Return a table on success, or 0 otherwise.
 * Caller must toml_free(the-return-value) after use.
 */
TOML_EXTERN toml_table_t* toml_parse(char* conf, /* NUL terminated, please. */
                                     char* errbuf,
                                     int errbufsz);

/* Free the table returned by toml_parse() or toml_parse_file(). */
TOML_EXTERN void toml_free(toml_table_t* tab);

/* Retrieve the key in table at keyidx. Return 0 if out of range. */
TOML_EXTERN const char* toml_key_in(toml_table_t* tab, int keyidx);

/* Lookup table by key. Return the element or 0 if not found. */
TOML_EXTERN const char* toml_raw_in(toml_table_t* tab, const char* key);
TOML_EXTERN toml_array_t* toml_array_in(toml_table_t* tab, const char* key);
TOML_EXTERN toml_table_t* toml_table_in(toml_table_t* tab, const char* key);

/* Return the array kind: 't'able, 'a'rray, 'v'alue */
TOML_EXTERN char toml_array_kind(toml_array_t* arr);

/* For array kind 'v'alue, return the type of values 
   i:int, d:double, b:bool, s:string, t:time, D:date, T:timestamp
   0 if unknown
*/
TOML_EXTERN char toml_array_type(toml_array_t* arr);


/* Return the number of elements in the array */
TOML_EXTERN int toml_array_nelem(toml_array_t* arr);

/* Return the key of an array */
TOML_EXTERN const char* toml_array_key(toml_array_t* arr);

/* Return the number of key-values in a table */
TOML_EXTERN int toml_table_nkval(toml_table_t* tab);

/* Return the number of arrays in a table */
TOML_EXTERN int toml_table_narr(toml_table_t* tab);

/* Return the number of sub-tables in a table */
TOML_EXTERN int toml_table_ntab(toml_table_t* tab);

/* Return the key of a table*/
TOML_EXTERN const char* toml_table_key(toml_table_t* tab);

/* Deref array by index. Return the element at idx or 0 if out of range. */
TOML_EXTERN const char* toml_raw_at(toml_array_t* arr, int idx);
TOML_EXTERN toml_array_t* toml_array_at(toml_array_t* arr, int idx);
TOML_EXTERN toml_table_t* toml_table_at(toml_array_t* arr, int idx);


/* Raw to String. Caller must call free(ret) after use. 
 * Return 0 on success, -1 otherwise.
 */
TOML_EXTERN int toml_rtos(const char* s, char** ret);

/* Raw to Boolean. Return 0 on success, -1 otherwise. */
TOML_EXTERN int toml_rtob(const char* s, int* ret);

/* Raw to Integer. Return 0 on success, -1 otherwise. */
TOML_EXTERN int toml_rtoi(const char* s, int64_t* ret);

/* Raw to Double. Return 0 on success, -1 otherwise. */
TOML_EXTERN int toml_rtod(const char* s, double* ret);
/* Same as toml_rtod, but return the sanitized double in string form as well */
TOML_EXTERN int toml_rtod_ex(const char* s, double* ret, char* buf, int buflen);

/* Timestamp types. The year, month, day, hour, minute, second, z 
 * fields may be NULL if they are not relevant. e.g. In a DATE
 * type, the hour, minute, second and z fields will be NULLs.
 */
typedef struct toml_timestamp_t toml_timestamp_t;
struct toml_timestamp_t {
    struct { /* internal. do not use. */
        int year, month, day;
        int hour, minute, second, millisec;
        char z[10];
    } __buffer;
    int *year, *month, *day;
    int *hour, *minute, *second, *millisec;
    char* z;
};

/* Raw to Timestamp. Return 0 on success, -1 otherwise. */
TOML_EXTERN int toml_rtots(const char* s, toml_timestamp_t* ret);

/* misc */
TOML_EXTERN int toml_utf8_to_ucs(const char* orig, int len, int64_t* ret);
TOML_EXTERN int toml_ucs_to_utf8(int64_t code, char buf[6]);


#endif /* TOML_H */
