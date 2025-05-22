module builtin

#include "@VEXEROOT/thirdparty/android/android.h"

// used by Android for (e)println to output to the Android log system / logcat
fn C.android_print(fstream voidptr, format &char, opt ...voidptr)
