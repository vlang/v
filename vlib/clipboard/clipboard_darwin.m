//NSPasteboard* darwin_new_pasteboard() {
void* darwin_new_pasteboard() {
	return (__bridge void*) [NSPasteboard generalPasteboard];
}

char* darwin_get_pasteboard_text(void* pb) {
	NSString *ns_clip = [((__bridge NSPasteboard*)pb) stringForType:NSStringPboardType]; //NSPasteboardTypeString
	if (ns_clip == nil) {
		return "";
	}
	return [ns_clip UTF8String];
}
