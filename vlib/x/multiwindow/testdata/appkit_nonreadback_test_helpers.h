#ifndef V_MULTIWINDOW_APPKIT_NONREADBACK_TEST_HELPERS_H
#define V_MULTIWINDOW_APPKIT_NONREADBACK_TEST_HELPERS_H

#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>

static IMP v_multiwindow_appkit_test_release_mouse_lock_original;
static Class v_multiwindow_appkit_test_release_mouse_lock_class;
static NSButton *v_multiwindow_appkit_test_accessibility_child;
static IMP v_multiwindow_appkit_test_release_services_original;
static Class v_multiwindow_appkit_test_release_services_class;
static int v_multiwindow_appkit_test_release_services_transitions;
static IMP v_multiwindow_appkit_test_clipboard_set_string_original;
static IMP v_multiwindow_appkit_test_clipboard_write_objects_original;
static IMP v_multiwindow_appkit_test_clipboard_general_original;
static Class v_multiwindow_appkit_test_clipboard_class;
static int v_multiwindow_appkit_test_clipboard_failure_mode;

static id v_multiwindow_appkit_test_clipboard_general_failure(id self, SEL command) {
	(void)self;
	(void)command;
	@throw [NSException exceptionWithName:@"VMultiwindowClipboardTestFailure"
		reason:@"injected generalPasteboard exception" userInfo:nil];
}

static BOOL v_multiwindow_appkit_test_clipboard_set_string_failure(id self,
		SEL command, NSString *value, NSPasteboardType type) {
	(void)value;
	if (v_multiwindow_appkit_test_clipboard_failure_mode == 2) {
		@throw [NSException exceptionWithName:@"VMultiwindowClipboardTestFailure"
			reason:@"injected setString exception" userInfo:nil];
	}
	if (v_multiwindow_appkit_test_clipboard_failure_mode == 3) {
		[(NSPasteboard *)self clearContents];
		(void)((BOOL (*)(id, SEL, NSString *, NSPasteboardType))
			v_multiwindow_appkit_test_clipboard_set_string_original)(self, command,
			@"external-takeover", type);
		return NO;
	}
	return NO;
}

static BOOL v_multiwindow_appkit_test_clipboard_write_objects_failure(id self,
		SEL command, NSArray *objects) {
	(void)self;
	(void)command;
	(void)objects;
	return NO;
}

static int v_multiwindow_appkit_test_install_clipboard_failure(int mode) {
	if (mode < 1 || mode > 5 || ![NSThread isMainThread]
			|| v_multiwindow_appkit_test_clipboard_set_string_original != NULL
			|| v_multiwindow_appkit_test_clipboard_general_original != NULL) {
		return 0;
	}
	NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	Class pasteboard_class = [pasteboard class];
	if (mode == 5) {
		Method general_method = class_getClassMethod([NSPasteboard class],
			@selector(generalPasteboard));
		if (general_method == NULL) {
			return 0;
		}
		v_multiwindow_appkit_test_clipboard_class = pasteboard_class;
		v_multiwindow_appkit_test_clipboard_failure_mode = mode;
		v_multiwindow_appkit_test_clipboard_general_original = method_setImplementation(
			general_method, (IMP)v_multiwindow_appkit_test_clipboard_general_failure);
		return v_multiwindow_appkit_test_clipboard_general_original != NULL ? 1 : 0;
	}
	Method set_method = class_getInstanceMethod(pasteboard_class,
		@selector(setString:forType:));
	Method write_method = class_getInstanceMethod(pasteboard_class,
		@selector(writeObjects:));
	if (set_method == NULL || write_method == NULL) {
		return 0;
	}
	v_multiwindow_appkit_test_clipboard_class = pasteboard_class;
	v_multiwindow_appkit_test_clipboard_failure_mode = mode;
	v_multiwindow_appkit_test_clipboard_set_string_original = method_setImplementation(
		set_method, (IMP)v_multiwindow_appkit_test_clipboard_set_string_failure);
	if (mode == 4) {
		v_multiwindow_appkit_test_clipboard_write_objects_original = method_setImplementation(
			write_method, (IMP)v_multiwindow_appkit_test_clipboard_write_objects_failure);
	}
	return v_multiwindow_appkit_test_clipboard_set_string_original != NULL
		&& (mode != 4 || v_multiwindow_appkit_test_clipboard_write_objects_original != NULL)
		? 1 : 0;
}

static void v_multiwindow_appkit_test_restore_clipboard_failure(void) {
	if (v_multiwindow_appkit_test_clipboard_class == Nil
			&& v_multiwindow_appkit_test_clipboard_general_original == NULL) {
		return;
	}
	Method general_method = class_getClassMethod([NSPasteboard class],
		@selector(generalPasteboard));
	if (general_method != NULL && v_multiwindow_appkit_test_clipboard_general_original != NULL) {
		method_setImplementation(general_method,
			v_multiwindow_appkit_test_clipboard_general_original);
	}
	Method set_method = class_getInstanceMethod(v_multiwindow_appkit_test_clipboard_class,
		@selector(setString:forType:));
	if (set_method != NULL && v_multiwindow_appkit_test_clipboard_set_string_original != NULL) {
		method_setImplementation(set_method,
			v_multiwindow_appkit_test_clipboard_set_string_original);
	}
	Method write_method = class_getInstanceMethod(v_multiwindow_appkit_test_clipboard_class,
		@selector(writeObjects:));
	if (write_method != NULL && v_multiwindow_appkit_test_clipboard_write_objects_original != NULL) {
		method_setImplementation(write_method,
			v_multiwindow_appkit_test_clipboard_write_objects_original);
	}
	v_multiwindow_appkit_test_clipboard_set_string_original = NULL;
	v_multiwindow_appkit_test_clipboard_write_objects_original = NULL;
	v_multiwindow_appkit_test_clipboard_general_original = NULL;
	v_multiwindow_appkit_test_clipboard_class = Nil;
	v_multiwindow_appkit_test_clipboard_failure_mode = 0;
}

static int v_multiwindow_appkit_test_seed_multitype_clipboard(void) {
	if (![NSThread isMainThread]) {
		return 0;
	}
	NSPasteboardItem *first = [[NSPasteboardItem alloc] init];
	NSPasteboardItem *second = [[NSPasteboardItem alloc] init];
	NSData *binary = [NSData dataWithBytes:(const unsigned char[]){0, 1, 2, 0xff}
		length:4];
	if (first == nil || second == nil
			|| ![first setString:@"original-text" forType:NSPasteboardTypeString]
			|| ![first setData:binary forType:@"org.vlang.multiwindow.binary"]
			|| ![second setString:@"second-item" forType:@"org.vlang.multiwindow.second"]) {
		return 0;
	}
	NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	[pasteboard clearContents];
	return [pasteboard writeObjects:@[ first, second ]] ? 1 : 0;
}

static int v_multiwindow_appkit_test_multitype_clipboard_matches_seed(void) {
	NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	NSArray<NSPasteboardItem *> *items = pasteboard.pasteboardItems;
	if (items.count != 2) {
		return 0;
	}
	NSData *binary = [NSData dataWithBytes:(const unsigned char[]){0, 1, 2, 0xff}
		length:4];
	return [[items[0] stringForType:NSPasteboardTypeString] isEqualToString:@"original-text"]
		&& [[items[0] dataForType:@"org.vlang.multiwindow.binary"] isEqualToData:binary]
		&& [[items[1] stringForType:@"org.vlang.multiwindow.second"]
			isEqualToString:@"second-item"] ? 1 : 0;
}

static int v_multiwindow_appkit_test_clipboard_has_external_takeover(void) {
	return [[[NSPasteboard generalPasteboard] stringForType:NSPasteboardTypeString]
		isEqualToString:@"external-takeover"] ? 1 : 0;
}

static BOOL v_multiwindow_appkit_test_release_mouse_lock_failure(id self, SEL command) {
	(void)self;
	(void)command;
	return NO;
}

static int v_multiwindow_appkit_test_install_release_mouse_lock_failure(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread] ||
		v_multiwindow_appkit_test_release_mouse_lock_original != NULL) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	id delegate = window.delegate;
	Method method = class_getInstanceMethod([delegate class], @selector(releaseMouseLock));
	if (delegate == nil || method == NULL) {
		return 0;
	}
	v_multiwindow_appkit_test_release_mouse_lock_class = [delegate class];
	v_multiwindow_appkit_test_release_mouse_lock_original = method_setImplementation(method,
		(IMP)v_multiwindow_appkit_test_release_mouse_lock_failure);
	return v_multiwindow_appkit_test_release_mouse_lock_original != NULL ? 1 : 0;
}

static void v_multiwindow_appkit_test_restore_release_mouse_lock(void) {
	if (v_multiwindow_appkit_test_release_mouse_lock_original == NULL ||
		v_multiwindow_appkit_test_release_mouse_lock_class == Nil) {
		return;
	}
	Method method = class_getInstanceMethod(v_multiwindow_appkit_test_release_mouse_lock_class,
		@selector(releaseMouseLock));
	if (method != NULL) {
		method_setImplementation(method,
			v_multiwindow_appkit_test_release_mouse_lock_original);
	}
	v_multiwindow_appkit_test_release_mouse_lock_original = NULL;
	v_multiwindow_appkit_test_release_mouse_lock_class = Nil;
}

static int v_multiwindow_appkit_test_window_is_visible(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	return ((__bridge NSWindow *)window_ptr).isVisible ? 1 : 0;
}

static int v_multiwindow_appkit_test_window_is_minimized(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	return ((__bridge NSWindow *)window_ptr).isMiniaturized ? 1 : 0;
}

static int v_multiwindow_appkit_test_attach_accessibility_child(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	v_multiwindow_appkit_test_accessibility_child = [[NSButton alloc]
		initWithFrame:NSMakeRect(0, 0, 10, 10)];
	v_multiwindow_appkit_test_accessibility_child.accessibilityParent = window;
	window.accessibilityChildren = @[v_multiwindow_appkit_test_accessibility_child];
	return v_multiwindow_appkit_test_accessibility_child.accessibilityParent == window ? 1 : 0;
}

static int v_multiwindow_appkit_test_accessibility_child_is_detached(void) {
	return v_multiwindow_appkit_test_accessibility_child != nil &&
		v_multiwindow_appkit_test_accessibility_child.accessibilityParent == nil ? 1 : 0;
}

static void v_multiwindow_appkit_test_release_accessibility_child(void) {
	v_multiwindow_appkit_test_accessibility_child = nil;
}

static int v_multiwindow_appkit_test_set_clipboard_ascii_payload(size_t length) {
	@autoreleasepool {
		NSMutableData *data = [NSMutableData dataWithLength:length];
		if (data == nil || (length > 0 && data.mutableBytes == NULL)) {
			return 0;
		}
		if (length > 0) {
			memset(data.mutableBytes, 'A', length);
		}
		NSString *value = [[NSString alloc] initWithData:data
			encoding:NSUTF8StringEncoding];
		if (value == nil) {
			return 0;
		}
		NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
		[pasteboard clearContents];
		return [pasteboard setString:value forType:NSPasteboardTypeString] ? 1 : 0;
	}
}

static int v_multiwindow_appkit_test_invoke_fullscreen_failure(void *window_ptr,
		int entering) {
	if (window_ptr == NULL || ![NSThread isMainThread] ||
		(entering != 0 && entering != 1)) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	id<NSWindowDelegate> delegate = window.delegate;
	if (delegate == nil) {
		return 0;
	}
	if (entering != 0) {
		if (![delegate respondsToSelector:@selector(windowDidFailToEnterFullScreen:)]) {
			return 0;
		}
		[delegate windowDidFailToEnterFullScreen:window];
		return 1;
	}
	if (![delegate respondsToSelector:@selector(windowDidFailToExitFullScreen:)]) {
		return 0;
	}
	[delegate windowDidFailToExitFullScreen:window];
	return 1;
}

static int v_multiwindow_appkit_test_invoke_metrics_notification(void *window_ptr,
		int notification_kind) {
	if (window_ptr == NULL || ![NSThread isMainThread] ||
		notification_kind < 1 || notification_kind > 3) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	if (notification_kind == 3) {
		[NSNotificationCenter.defaultCenter
			postNotificationName:NSApplicationDidChangeScreenParametersNotification
			object:NSApp];
		return 1;
	}
	id<NSWindowDelegate> delegate = window.delegate;
	if (delegate == nil) {
		return 0;
	}
	if (notification_kind == 1) {
		if (![delegate respondsToSelector:@selector(windowDidChangeScreen:)]) {
			return 0;
		}
		NSNotification *notification = [NSNotification
			notificationWithName:NSWindowDidChangeScreenNotification object:window];
		[delegate windowDidChangeScreen:notification];
		return 1;
	}
	if (![delegate respondsToSelector:@selector(windowDidChangeBackingProperties:)]) {
		return 0;
	}
	NSNotification *notification = [NSNotification
		notificationWithName:NSWindowDidChangeBackingPropertiesNotification object:window];
	[delegate windowDidChangeBackingProperties:notification];
	return 1;
}

static int v_multiwindow_appkit_test_change_bounds_and_invoke_metrics_notification(
		void *window_ptr, int notification_kind, int width, int height) {
	if (window_ptr == NULL || ![NSThread isMainThread] ||
		(notification_kind != 1 && notification_kind != 2) ||
		width <= 0 || height <= 0) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	NSView *content = window.contentView;
	id<NSWindowDelegate> delegate = window.delegate;
	if (content == nil || delegate == nil) {
		return 0;
	}
	[content setBoundsSize:NSMakeSize((CGFloat)width, (CGFloat)height)];
	if (notification_kind == 1) {
		if (![delegate respondsToSelector:@selector(windowDidChangeScreen:)]) {
			return 0;
		}
		NSNotification *notification = [NSNotification
			notificationWithName:NSWindowDidChangeScreenNotification object:window];
		[delegate windowDidChangeScreen:notification];
	} else {
		if (![delegate respondsToSelector:@selector(windowDidChangeBackingProperties:)]) {
			return 0;
		}
		NSNotification *notification = [NSNotification
			notificationWithName:NSWindowDidChangeBackingPropertiesNotification object:window];
		[delegate windowDidChangeBackingProperties:notification];
	}
	NSSize actual = content.bounds.size;
	return ((int)lrint(actual.width) == width &&
		(int)lrint(actual.height) == height) ? 1 : 0;
}

static int v_multiwindow_appkit_test_window_identities(void *window_ptr,
		uint64_t *out_window, uint64_t *out_private_handle,
		uint64_t *out_first_responder) {
	if (window_ptr == NULL || out_window == NULL || out_private_handle == NULL ||
		out_first_responder == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	NSView *content = window.contentView;
	id private_handle = window.delegate;
	if (content == nil || private_handle == nil) {
		return 0;
	}
	NSTextField *field = [[NSTextField alloc] initWithFrame:NSMakeRect(0, 0, 16, 16)];
	[content addSubview:field];
	if (![window makeFirstResponder:field] || window.firstResponder == nil) {
		[field removeFromSuperview];
		return 0;
	}
	*out_window = (uint64_t)(uintptr_t)(__bridge void *)window;
	*out_private_handle = (uint64_t)(uintptr_t)(__bridge void *)private_handle;
	*out_first_responder = (uint64_t)(uintptr_t)(__bridge void *)window.firstResponder;
	return 1;
}

static int v_multiwindow_appkit_test_current_first_responder(void *window_ptr,
		uint64_t *out_first_responder) {
	if (window_ptr == NULL || out_first_responder == NULL ||
		![NSThread isMainThread]) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	if (window.firstResponder == nil) {
		return 0;
	}
	*out_first_responder =
		(uint64_t)(uintptr_t)(__bridge void *)window.firstResponder;
	return 1;
}

static int v_multiwindow_appkit_test_close_window(void *window_ptr,
		int perform_close) {
	if (window_ptr == NULL || ![NSThread isMainThread] ||
		(perform_close != 0 && perform_close != 1)) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	if (perform_close != 0) {
		[window performClose:nil];
	} else {
		[window close];
	}
	return 1;
}

static void v_multiwindow_appkit_test_count_release_services(id self, SEL command) {
	BOOL was_released = [[self valueForKey:@"serviceReleased"] boolValue];
	((void (*)(id, SEL))v_multiwindow_appkit_test_release_services_original)(self,
		command);
	BOOL is_released = [[self valueForKey:@"serviceReleased"] boolValue];
	if (!was_released && is_released) {
		v_multiwindow_appkit_test_release_services_transitions++;
	}
}

static int v_multiwindow_appkit_test_install_release_services_counter(
		void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread] ||
		v_multiwindow_appkit_test_release_services_original != NULL) {
		return 0;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	id delegate = window.delegate;
	SEL selector = NSSelectorFromString(@"releaseServices");
	Method method = delegate != nil
		? class_getInstanceMethod([delegate class], selector) : NULL;
	if (method == NULL) {
		return 0;
	}
	v_multiwindow_appkit_test_release_services_class = [delegate class];
	v_multiwindow_appkit_test_release_services_transitions = 0;
	v_multiwindow_appkit_test_release_services_original = method_setImplementation(
		method, (IMP)v_multiwindow_appkit_test_count_release_services);
	return v_multiwindow_appkit_test_release_services_original != NULL ? 1 : 0;
}

static int v_multiwindow_appkit_test_release_services_count(void) {
	return v_multiwindow_appkit_test_release_services_transitions;
}

static void v_multiwindow_appkit_test_restore_release_services_counter(void) {
	if (v_multiwindow_appkit_test_release_services_original == NULL ||
		v_multiwindow_appkit_test_release_services_class == Nil) {
		return;
	}
	Method method = class_getInstanceMethod(
		v_multiwindow_appkit_test_release_services_class,
		NSSelectorFromString(@"releaseServices"));
	if (method != NULL) {
		method_setImplementation(method,
			v_multiwindow_appkit_test_release_services_original);
	}
	v_multiwindow_appkit_test_release_services_original = NULL;
	v_multiwindow_appkit_test_release_services_class = Nil;
}

#endif
