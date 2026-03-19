#include <stdbool.h>
#include <objc/message.h>
#include <objc/runtime.h>

typedef struct macos_rect {
	double x;
	double y;
	double width;
	double height;
} macos_rect;

static inline void* macos_objc_get_class(const char* name) {
	return (__bridge void*)objc_getClass(name);
}

static inline void* macos_objc_get_protocol(const char* name) {
	return (__bridge void*)objc_getProtocol(name);
}

static inline void* macos_sel_register_name(const char* name) {
	return (void*)sel_registerName(name);
}

static inline void* macos_objc_allocate_class_pair(void* superclass, const char* name, size_t extra_bytes) {
	return (__bridge void*)objc_allocateClassPair((__bridge Class)superclass, name, extra_bytes);
}

static inline void macos_objc_register_class_pair(void* cls) {
	objc_registerClassPair((__bridge Class)cls);
}

static inline bool macos_class_add_method(void* cls, void* name, void* imp, const char* types) {
	return class_addMethod((__bridge Class)cls, (SEL)name, (IMP)imp, types);
}

static inline bool macos_class_add_ivar(void* cls, const char* name, size_t size, uint8_t alignment, const char* types) {
	return class_addIvar((__bridge Class)cls, name, size, alignment, types);
}

static inline bool macos_class_add_protocol(void* cls, void* protocol) {
	return class_addProtocol((__bridge Class)cls, (__bridge Protocol*)protocol);
}

static inline void macos_set_associated_object(void* obj, const void* key, void* value, uintptr_t policy) {
	objc_setAssociatedObject((__bridge id)obj, key, (__bridge id)value, (objc_AssociationPolicy)policy);
}

static inline void* macos_get_associated_object(void* obj, const void* key) {
	return (__bridge void*)objc_getAssociatedObject((__bridge id)obj, key);
}

static inline void macos_objc_set_ptr_ivar(void* obj, const char* name, void* value) {
	if (obj == NULL) {
		return;
	}
	Ivar ivar = class_getInstanceVariable(object_getClass((__bridge id)obj), name);
	if (ivar == NULL) {
		return;
	}
	ptrdiff_t offset = ivar_getOffset(ivar);
	*(void**)(((char*)obj) + offset) = value;
}

static inline void* macos_objc_get_ptr_ivar(void* obj, const char* name) {
	if (obj == NULL) {
		return NULL;
	}
	Ivar ivar = class_getInstanceVariable(object_getClass((__bridge id)obj), name);
	if (ivar == NULL) {
		return NULL;
	}
	ptrdiff_t offset = ivar_getOffset(ivar);
	return *(void**)(((char*)obj) + offset);
}

static inline void* macos_objc_msg_id0(void* obj, void* sel) {
	return ((void* (*)(void*, void*))objc_msgSend)(obj, sel);
}

static inline void* macos_objc_msg_id1(void* obj, void* sel, void* a0) {
	return ((void* (*)(void*, void*, void*))objc_msgSend)(obj, sel, a0);
}

static inline void* macos_objc_msg_id2(void* obj, void* sel, void* a0, void* a1) {
	return ((void* (*)(void*, void*, void*, void*))objc_msgSend)(obj, sel, a0, a1);
}

static inline void* macos_objc_msg_id3(void* obj, void* sel, void* a0, void* a1, void* a2) {
	return ((void* (*)(void*, void*, void*, void*, void*))objc_msgSend)(obj, sel, a0, a1, a2);
}

static inline void* macos_objc_msg_id4(void* obj, void* sel, void* a0, void* a1, void* a2, void* a3) {
	return ((void* (*)(void*, void*, void*, void*, void*, void*))objc_msgSend)(obj, sel, a0, a1, a2, a3);
}

static inline void* macos_objc_msg_id_rect(void* obj, void* sel, macos_rect rect) {
	return ((void* (*)(void*, void*, macos_rect))objc_msgSend)(obj, sel, rect);
}

static inline void* macos_objc_msg_id_rect_bool(void* obj, void* sel, macos_rect rect, bool a1) {
	return ((void* (*)(void*, void*, macos_rect, bool))objc_msgSend)(obj, sel, rect, a1);
}

static inline void* macos_objc_msg_id_rect_obj(void* obj, void* sel, macos_rect rect, void* a1) {
	return ((void* (*)(void*, void*, macos_rect, void*))objc_msgSend)(obj, sel, rect, a1);
}

static inline void* macos_objc_msg_id_rect_u64_u64_bool(void* obj, void* sel, macos_rect rect, unsigned long long a1, unsigned long long a2, bool a3) {
	return ((void* (*)(void*, void*, macos_rect, unsigned long long, unsigned long long, bool))objc_msgSend)(obj, sel, rect, a1, a2, a3);
}

static inline void* macos_objc_msg_id_obj_u64_bool(void* obj, void* sel, void* a0, unsigned long long a1, bool a2) {
	return ((void* (*)(void*, void*, void*, unsigned long long, bool))objc_msgSend)(obj, sel, a0, a1, a2);
}

static inline void* macos_objc_msg_id_obj_sel_obj(void* obj, void* sel, void* a0, void* a1, void* a2) {
	return ((void* (*)(void*, void*, void*, void*, void*))objc_msgSend)(obj, sel, a0, a1, a2);
}

static inline void* macos_objc_msg_id_f64(void* obj, void* sel, double a0) {
	return ((void* (*)(void*, void*, double))objc_msgSend)(obj, sel, a0);
}

static inline void* macos_objc_msg_id_u64(void* obj, void* sel, unsigned long long a0) {
	return ((void* (*)(void*, void*, unsigned long long))objc_msgSend)(obj, sel, a0);
}

static inline void macos_objc_msg_void0(void* obj, void* sel) {
	((void (*)(void*, void*))objc_msgSend)(obj, sel);
}

static inline void macos_objc_msg_void1(void* obj, void* sel, void* a0) {
	((void (*)(void*, void*, void*))objc_msgSend)(obj, sel, a0);
}

static inline void macos_objc_msg_void2(void* obj, void* sel, void* a0, void* a1) {
	((void (*)(void*, void*, void*, void*))objc_msgSend)(obj, sel, a0, a1);
}

static inline void macos_objc_msg_void3(void* obj, void* sel, void* a0, void* a1, void* a2) {
	((void (*)(void*, void*, void*, void*, void*))objc_msgSend)(obj, sel, a0, a1, a2);
}

static inline void macos_objc_msg_void_bool(void* obj, void* sel, bool a0) {
	((void (*)(void*, void*, bool))objc_msgSend)(obj, sel, a0);
}

static inline void macos_objc_msg_void_i64(void* obj, void* sel, long long a0) {
	((void (*)(void*, void*, long long))objc_msgSend)(obj, sel, a0);
}

static inline void macos_objc_msg_void_u64(void* obj, void* sel, unsigned long long a0) {
	((void (*)(void*, void*, unsigned long long))objc_msgSend)(obj, sel, a0);
}

static inline void macos_objc_msg_void_f64(void* obj, void* sel, double a0) {
	((void (*)(void*, void*, double))objc_msgSend)(obj, sel, a0);
}

static inline void macos_objc_msg_void_rect(void* obj, void* sel, macos_rect rect) {
	((void (*)(void*, void*, macos_rect))objc_msgSend)(obj, sel, rect);
}

static inline void macos_objc_msg_void_rect_bool_bool(void* obj, void* sel, macos_rect rect, bool a1, bool a2) {
	((void (*)(void*, void*, macos_rect, bool, bool))objc_msgSend)(obj, sel, rect, a1, a2);
}

static inline bool macos_objc_msg_bool0(void* obj, void* sel) {
	return ((bool (*)(void*, void*))objc_msgSend)(obj, sel);
}

static inline bool macos_objc_msg_bool1(void* obj, void* sel, void* a0) {
	return ((bool (*)(void*, void*, void*))objc_msgSend)(obj, sel, a0);
}

static inline long long macos_objc_msg_i64(void* obj, void* sel) {
	return ((long long (*)(void*, void*))objc_msgSend)(obj, sel);
}

static inline unsigned long long macos_objc_msg_u64(void* obj, void* sel) {
	return ((unsigned long long (*)(void*, void*))objc_msgSend)(obj, sel);
}

static inline double macos_objc_msg_f64(void* obj, void* sel) {
	return ((double (*)(void*, void*))objc_msgSend)(obj, sel);
}

static inline macos_rect macos_objc_msg_rect(void* obj, void* sel) {
	return ((macos_rect (*)(void*, void*))objc_msgSend)(obj, sel);
}
