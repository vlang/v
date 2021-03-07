
@implementation MyView2

// Alternative drawRect which calls a frame function with native Cocoa calls
- (void)drawRect:(NSRect)rect {
    _sapp_call_frame2();
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


