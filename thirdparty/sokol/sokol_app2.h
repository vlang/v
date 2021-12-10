
@implementation MyView2

int __v_sokol_inited = 0;

// Alternative drawRect which calls a frame function with native Cocoa calls
- (void)drawRect:(NSRect)rect {
	//puts("drawRect()");
	if (__v_sokol_inited == 0) {
        _sapp_call_init();
       __v_sokol_inited = 1;
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


