module builtin

#flag android @VEXEROOT/vlib/builtin/android_print.o
#flag android -llog

fn C.__android_log_print(priority i32, const_tag &char, const_format &char, opt ...voidptr) i32
