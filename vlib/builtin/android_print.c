#include <stdio.h>
#include <stdarg.h>
#include <android/log.h>

#define V_ANDROID_LOG_STR_VALUE(arg) #arg
#define V_ANDROID_LOG_NAME(tag_name) V_ANDROID_LOG_STR_VALUE(tag_name)
#ifndef V_ANDROID_LOG_TAG
#if defined(APPNAME)
#define V_ANDROID_LOG_TAG APPNAME
#else
#define V_ANDROID_LOG_TAG "V"
#endif
#endif
#define V_ANDROID_LOG_TAG_NAME V_ANDROID_LOG_NAME(V_ANDROID_LOG_TAG)

int android_print(void *stream, const char *format, ...) {
	va_list args;
	va_start(args, format);
	int priority = stream == stdout ? ANDROID_LOG_INFO : ANDROID_LOG_ERROR;
	int result = __android_log_vprint(priority, V_ANDROID_LOG_TAG_NAME, format, args);
	va_end(args);
	return result;
}
