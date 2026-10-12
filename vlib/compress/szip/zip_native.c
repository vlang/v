#include "../../../thirdparty/zip/zip.c"

_Static_assert(MZ_NO_COMPRESSION == 0 && MZ_BEST_SPEED == 1 && MZ_BEST_COMPRESSION == 9 &&
	MZ_UBER_COMPRESSION == 10 && MZ_DEFAULT_LEVEL == 6 && MZ_DEFAULT_COMPRESSION == -1,
	"V zip compression levels match the bundled miniz ABI");
