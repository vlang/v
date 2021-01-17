#if defined(__ANDROID__)
	// Adapted from https://stackoverflow.com/a/196018/1904615
	#define V_ANDROID_LOG_STR_VALUE(arg) #arg
	#define V_ANDROID_LOG_NAME(tag_name) V_ANDROID_LOG_STR_VALUE(tag_name)

	#ifndef V_ANDROID_LOG_TAG
		#if defined(APPNAME)
			#define V_ANDROID_LOG_TAG APPNAME
		#else
			#define V_ANDROID_LOG_TAG "V_ANDROID"
		#endif
	#endif

	#define V_ANDROID_LOG_TAG_NAME V_ANDROID_LOG_NAME(V_ANDROID_LOG_TAG)

	#include <android/log.h>
	#define printf(...) __android_log_print(ANDROID_LOG_INFO, V_ANDROID_LOG_TAG_NAME, __VA_ARGS__)
	#define fprintf(a, ...) __android_log_print(ANDROID_LOG_ERROR, V_ANDROID_LOG_TAG_NAME, __VA_ARGS__)
#endif

