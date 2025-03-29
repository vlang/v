#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct {
  char **array_ptr;
  int array_len;
  int string_len;
} array_t;

typedef enum flag_bits {
	AAA = 0,
	BBB = 1,
	CCC = 2
} flag_bits;

typedef enum flag_bits_main {
	AAAA = 0,
	BBBB = 1,
	CCCC = 2
} flag_bits_main;

typedef struct {
  array_t arr[4];
  flag_bits enu[2];
} struct_array;
typedef uint32_t flags;

typedef union union_t {
    float       float32[4];
    int32_t     int32[4];
    uint32_t    uint32[4];
} union_t;

array_t*
array_string_new(int array_len, int string_len)
{
  int i;
  char **array_ptr = (char**) malloc(array_len * sizeof(char**));

  for(i = 0; i < array_len; i++) {
    array_ptr[i] = (char*) malloc(string_len * sizeof(char));
  }

  array_t *array = (array_t*) malloc(sizeof(array_t));
  array->array_ptr = array_ptr;
  array->array_len = array_len;
  array->string_len = string_len;

  return array;
}

int
array_string_set(array_t *array, int index, char *string)
{
  strncpy(array->array_ptr[index], string, array->string_len);
  return 0;
}

char*
array_string_get(array_t *array, int index)
{
  return array->array_ptr[index];
}

int
array_string_len(array_t *array)
{
  return array->array_len;
}

int
array_string_free(array_t *array)
{
  int i;
  for(i = 0; i < array->array_len; i++) {
    free(array->array_ptr[i]);
  }
  free(array->array_ptr);
  return 0;
}

char** get_string_array() {
  array_t *array = array_string_new(4, 4);

  array_string_set(array, 0, "foo");
  array_string_set(array, 1, "bar");
  array_string_set(array, 2, "bat");
  array_string_set(array, 3, ".... overflowed string");

	return array->array_ptr;
}

struct_array get_struct_array() {
  int i;
  array_t* array = array_string_new(4, 4);

  array_string_set(array, 0, "foo");
  array_string_set(array, 1, "bar");
  array_string_set(array, 2, "bat");
  array_string_set(array, 3, ".... overflowed string");

  struct_array ret;
  for(i = 0; i < array_string_len(array); i++) {
    ret.arr[i] = *array;
  };
  free(array);
  
  return ret;
}

flags get_enum() {
  flag_bits ret = AAA;
  return ret;
}

union_t get_union() {
  uint32_t r = 123;
	union_t ret;
	ret.uint32[0] = r;
	ret.uint32[1] = r;
	ret.uint32[2] = r;
	ret.uint32[3] = r;
	return ret;
}

void get_stub(flag_bits param[2]) {}

void get_stub_main(flag_bits_main param[2]) {}

void
set_struct_array(struct_array* param)
{
  puts("\nset struct_array\n");
}