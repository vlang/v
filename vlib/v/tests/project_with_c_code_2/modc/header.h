#ifndef CMOD_H
#define CMOD_H

struct Atype {
	int val;
};


void* new_atype(int n);

void handle_array(void *p, int n);

void handle_array2(void *p, int n);

void destroy_atype(void *p);


// The following emulates the structure of the SDL3 header SDL_pixels.h, and the enum SDL_PixelFormat.
// See also https://github.com/vlang/v/issues/25135 .
#define ABC 1
#define XYZ 1

typedef enum EnumWithDuplicates {
    UNKNOWN = 0,
    NAME1 = 0x1u,
    NAME2 = 0x2u,
    NAME3 = 0x3u,
    NAME4 = 0x4u,
    NAME5 = 0x5u,
    NAME6 = 0x6u,
    #if ABC == XYZ
	    COMMON_NAME1 = NAME1,
	    COMMON_NAME2 = NAME2,
	    COMMON_NAME3 = NAME3
    #else
	    COMMON_NAME1 = NAME4,
	    COMMON_NAME2 = NAME5,
	    COMMON_NAME3 = NAME6
    #endif
} EnumWithDuplicates;

#endif

