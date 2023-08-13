// This file should be compiled with a C++ compiler:
extern "C" {
	int sizeof_char(void);
}

int sizeof_char(void) {
   // see https://stackoverflow.com/a/12887719/1023403
   return sizeof('a'); // 4 for C compilers, 1 for C++ compilers
}
