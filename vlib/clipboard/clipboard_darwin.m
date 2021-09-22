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

bool darwin_set_pasteboard_text(void* _pb, string text) {
	NSPasteboard* pb = (__bridge NSPasteboard*) _pb;
	NSString *ns_clip = [[ NSString alloc ] initWithBytesNoCopy:text.str length:text.len encoding:NSUTF8StringEncoding freeWhenDone: false];
	[pb declareTypes:[NSArray arrayWithObject:NSStringPboardType] owner:nil];
	bool ret = [pb setString:ns_clip forType:NSStringPboardType];
	//[ns_clip release];
	int serial = [pb changeCount];
	//OSAtomicCompareAndSwapLong(cb.last_cb_serial, serial, &cb.last_cb_serial);
	return ret;
}
