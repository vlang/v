
@implementation MyView2

int xx = 0;

// Alternative drawRect which calls a frame function with native Cocoa calls
- (void)drawRect:(NSRect)rect {
	puts("drawRect()");
	if (xx == 0) {
        _sapp_call_init();
       xx = 1;
      }
    _sapp_call_frame_native();
}

//- (BOOL)isOpaque {
//    return NO;
//}

- (BOOL)canBecomeKeyView {
    return YES;
}
- (BOOL)acceptsFirstResponder {
    return YES;
}

// - (void)mouseExited:(NSEvent*)event {
// }

// - (void)mouseDown:(NSEvent*)event {
// }

- (BOOL)acceptsFirstMouse:(NSEvent *)event {
	return YES;
}


@end


