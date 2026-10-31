// This file should be compiled with a C++ compiler:
#include <type_traits>

extern "C" {
	int sizeof_char(void);
	int cplusplus_17_feature(void);
}

int sizeof_char(void) {
   // see https://stackoverflow.com/a/12887719/1023403
   return sizeof('a'); // 4 for C compilers, 1 for C++ compilers
}

// `if constexpr` and `std::is_same_v` need C++17, the standard V uses for C++ objects.
template <typename T>
constexpr int cplusplus_17_kind() {
	if constexpr (std::is_same_v<T, char>) {
		return 17;
	} else {
		return 0;
	}
}

int cplusplus_17_feature(void) {
	return cplusplus_17_kind<char>();
}
