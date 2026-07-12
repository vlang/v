#ifndef V_MULTIWINDOW_X11_EGL_BACKEND_HELPERS_H
#define V_MULTIWINDOW_X11_EGL_BACKEND_HELPERS_H

#include <stdint.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>
#include <X11/Xutil.h>
#include <EGL/egl.h>

#ifndef EGL_CONTEXT_MAJOR_VERSION
#define EGL_CONTEXT_MAJOR_VERSION 0x3098
#endif
#ifndef EGL_CONTEXT_MINOR_VERSION
#define EGL_CONTEXT_MINOR_VERSION 0x30FB
#endif
#ifndef EGL_CONTEXT_OPENGL_PROFILE_MASK
#define EGL_CONTEXT_OPENGL_PROFILE_MASK 0x30FD
#endif
#ifndef EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT
#define EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT 0x00000001
#endif

#ifndef MWM_HINTS_DECORATIONS
#define MWM_HINTS_DECORATIONS (1L << 1)
#endif

#ifndef V_MULTIWINDOW_X11_XIM_STACK_BYTES
#define V_MULTIWINDOW_X11_XIM_STACK_BYTES 128
#endif
#ifndef V_MULTIWINDOW_X11_XIM_MAX_BYTES
#define V_MULTIWINDOW_X11_XIM_MAX_BYTES 32768
#endif

#define V_MULTIWINDOW_CURSOR_SHAPE_DEFAULT 0
#define V_MULTIWINDOW_CURSOR_SHAPE_POINTER 1
#define V_MULTIWINDOW_CURSOR_SHAPE_MOVE 2
#define V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE 3
#define V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE 4
#define V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE 5
#define V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE 6
#define V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE 7
#define V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE 8
#define V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE 9
#define V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE 10
#define V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE 11
#define V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE 12
#define V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE 13
#define V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE 14
#define V_MULTIWINDOW_CURSOR_SHAPE_GRAB 15
#define V_MULTIWINDOW_CURSOR_SHAPE_GRABBING 16

typedef struct {
	unsigned long flags;
	unsigned long functions;
	unsigned long decorations;
	long input_mode;
	unsigned long status;
} VMultiwindowMotifWmHints;

typedef struct {
	int key;
	char name[XkbKeyNameLength];
} VMultiwindowX11KeymapEntry;

#define V_MULTIWINDOW_X11_KEYMAP_ENTRY(key, a, b, c, d) { key, { a, b, c, d } }

static inline long v_multiwindow_x11_event_mask(void) {
	return StructureNotifyMask |
		PropertyChangeMask |
		KeyPressMask |
		KeyReleaseMask |
		PointerMotionMask |
		ButtonPressMask |
		ButtonReleaseMask |
		FocusChangeMask |
		EnterWindowMask |
		LeaveWindowMask;
}

static inline unsigned long v_multiwindow_x11_event_window(XEvent *event) {
	switch (event->type) {
	case KeyPress:
	case KeyRelease:
		return (unsigned long)event->xkey.window;
	case ButtonPress:
	case ButtonRelease:
		return (unsigned long)event->xbutton.window;
	case MotionNotify:
		return (unsigned long)event->xmotion.window;
	case EnterNotify:
	case LeaveNotify:
		return (unsigned long)event->xcrossing.window;
	case FocusIn:
	case FocusOut:
		return (unsigned long)event->xfocus.window;
	case ClientMessage:
		return (unsigned long)event->xclient.window;
	case ConfigureNotify:
		return (unsigned long)event->xconfigure.window;
	case DestroyNotify:
		return (unsigned long)event->xdestroywindow.window;
	case PropertyNotify:
		return (unsigned long)event->xproperty.window;
	default:
		return 0;
	}
}

static inline unsigned long v_multiwindow_x11_property_atom(XEvent *event) {
	return event->type == PropertyNotify ? (unsigned long)event->xproperty.atom : 0;
}

static inline int v_multiwindow_x11_property_state(XEvent *event) {
	return event->type == PropertyNotify ? event->xproperty.state : -1;
}

static inline int v_multiwindow_x11_event_x(XEvent *event) {
	switch (event->type) {
	case ButtonPress:
	case ButtonRelease:
		return event->xbutton.x;
	case MotionNotify:
		return event->xmotion.x;
	case EnterNotify:
	case LeaveNotify:
		return event->xcrossing.x;
	default:
		return 0;
	}
}

static inline int v_multiwindow_x11_event_y(XEvent *event) {
	switch (event->type) {
	case ButtonPress:
	case ButtonRelease:
		return event->xbutton.y;
	case MotionNotify:
		return event->xmotion.y;
	case EnterNotify:
	case LeaveNotify:
		return event->xcrossing.y;
	default:
		return 0;
	}
}

static inline unsigned int v_multiwindow_x11_event_state(XEvent *event) {
	switch (event->type) {
	case KeyPress:
	case KeyRelease:
		return event->xkey.state;
	case ButtonPress:
	case ButtonRelease:
		return event->xbutton.state;
	case MotionNotify:
		return event->xmotion.state;
	case EnterNotify:
	case LeaveNotify:
		return event->xcrossing.state;
	default:
		return 0;
	}
}

static inline unsigned int v_multiwindow_x11_event_keycode(XEvent *event) {
	return (event->type == KeyPress || event->type == KeyRelease) ? event->xkey.keycode : 0;
}

static inline unsigned int v_multiwindow_x11_event_button(XEvent *event) {
	return (event->type == ButtonPress || event->type == ButtonRelease) ? event->xbutton.button : 0;
}

static inline int v_multiwindow_x11_focus_mode(XEvent *event) {
	return (event->type == FocusIn || event->type == FocusOut) ? event->xfocus.mode : -1;
}

static inline int v_multiwindow_x11_is_notify_grab_or_ungrab(int mode) {
	return mode == NotifyGrab || mode == NotifyUngrab;
}

static inline int v_multiwindow_x11_enable_detectable_auto_repeat(Display *display) {
	if (display == NULL) {
		return 0;
	}
	Bool supported = False;
	if (XkbSetDetectableAutoRepeat(display, True, &supported) != True) {
		return 0;
	}
	return supported ? 1 : 0;
}

static inline int v_multiwindow_x11_is_auto_repeat_release(Display *display, XEvent *event) {
	if (display == NULL || event == NULL || event->type != KeyRelease || XPending(display) <= 0) {
		return 0;
	}
	XEvent next;
	XPeekEvent(display, &next);
	if (next.type != KeyPress) {
		return 0;
	}
	return next.xkey.window == event->xkey.window &&
		next.xkey.keycode == event->xkey.keycode &&
		next.xkey.time == event->xkey.time;
}

static inline int v_multiwindow_x11_modifiers(unsigned int state) {
	int modifiers = 0;
	if (state & ShiftMask) {
		modifiers |= 1;
	}
	if (state & ControlMask) {
		modifiers |= 2;
	}
	if (state & Mod1Mask) {
		modifiers |= 4;
	}
	if (state & Mod4Mask) {
		modifiers |= 8;
	}
	if (state & Button1Mask) {
		modifiers |= 0x100;
	}
	if (state & Button3Mask) {
		modifiers |= 0x200;
	}
	if (state & Button2Mask) {
		modifiers |= 0x400;
	}
	return modifiers;
}

static inline int v_multiwindow_x11_key_modifier_bit(int key_code) {
	switch (key_code) {
	case 340:
	case 344:
		return 1;
	case 341:
	case 345:
		return 2;
	case 342:
	case 346:
		return 4;
	case 343:
	case 347:
		return 8;
	default:
		return 0;
	}
}

static inline int v_multiwindow_x11_mouse_button(unsigned int button) {
	switch (button) {
	case Button1:
		return 0;
	case Button3:
		return 1;
	case Button2:
		return 2;
	default:
		return 256;
	}
}

static inline int v_multiwindow_x11_button_modifier_bit(int mouse_button) {
	switch (mouse_button) {
	case 0:
		return 0x100;
	case 1:
		return 0x200;
	case 2:
		return 0x400;
	default:
		return 0;
	}
}

static inline unsigned int v_multiwindow_x11_cursor_font_shape(int shape) {
	switch (shape) {
	case V_MULTIWINDOW_CURSOR_SHAPE_POINTER:
		return XC_hand2;
	case V_MULTIWINDOW_CURSOR_SHAPE_MOVE:
	case V_MULTIWINDOW_CURSOR_SHAPE_GRAB:
	case V_MULTIWINDOW_CURSOR_SHAPE_GRABBING:
		return XC_fleur;
	case V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE:
		return XC_top_side;
	case V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE:
		return XC_bottom_side;
	case V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE:
		return XC_right_side;
	case V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE:
		return XC_left_side;
	case V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE:
		return XC_top_right_corner;
	case V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE:
		return XC_top_left_corner;
	case V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE:
		return XC_bottom_right_corner;
	case V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE:
		return XC_bottom_left_corner;
	case V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE:
		return XC_sb_h_double_arrow;
	case V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE:
		return XC_sb_v_double_arrow;
	case V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE:
		return XC_top_right_corner;
	case V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE:
		return XC_top_left_corner;
	default:
		return XC_left_ptr;
	}
}

static inline unsigned long v_multiwindow_x11_create_cursor_for_shape(Display *display, int shape) {
	if (display == NULL) {
		return 0;
	}
	return (unsigned long)XCreateFontCursor(display, v_multiwindow_x11_cursor_font_shape(shape));
}

static inline XIM v_multiwindow_x11_open_im(Display *display) {
	if (display == NULL) {
		return NULL;
	}
	setlocale(LC_CTYPE, "");
	XSetLocaleModifiers("");
	XIM im = XOpenIM(display, NULL, NULL, NULL);
	if (im == NULL) {
		XSetLocaleModifiers("@im=none");
		im = XOpenIM(display, NULL, NULL, NULL);
	}
	return im;
}

static inline void v_multiwindow_x11_close_im(XIM im) {
	if (im != NULL) {
		XCloseIM(im);
	}
}

static inline XIC v_multiwindow_x11_create_ic(XIM im, unsigned long window) {
	if (im == NULL || window == 0) {
		return NULL;
	}
	return XCreateIC(im,
		XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
		XNClientWindow, (Window)window,
		XNFocusWindow, (Window)window,
		NULL);
}

static inline void v_multiwindow_x11_destroy_ic(XIC ic) {
	if (ic != NULL) {
		XDestroyIC(ic);
	}
}

static inline void v_multiwindow_x11_set_ic_focus(XIC ic) {
	if (ic != NULL) {
		XSetICFocus(ic);
	}
}

static inline void v_multiwindow_x11_unset_ic_focus(XIC ic) {
	if (ic != NULL) {
		XUnsetICFocus(ic);
	}
}

static inline int v_multiwindow_x11_key_code_from_keysym(KeySym keysym) {
	if (keysym >= XK_a && keysym <= XK_z) {
		return (int)(keysym - XK_a + 65);
	}
	if (keysym >= XK_A && keysym <= XK_Z) {
		return (int)(keysym - XK_A + 65);
	}
	if (keysym >= XK_0 && keysym <= XK_9) {
		return (int)(keysym - XK_0 + 48);
	}
	if (keysym >= XK_F1 && keysym <= XK_F25) {
		return (int)(keysym - XK_F1 + 290);
	}
	if (keysym >= XK_KP_0 && keysym <= XK_KP_9) {
		return (int)(keysym - XK_KP_0 + 320);
	}
	switch (keysym) {
	case XK_space:
		return 32;
	case XK_apostrophe:
		return 39;
	case XK_comma:
		return 44;
	case XK_minus:
		return 45;
	case XK_period:
		return 46;
	case XK_slash:
		return 47;
	case XK_semicolon:
		return 59;
	case XK_equal:
		return 61;
	case XK_bracketleft:
		return 91;
	case XK_backslash:
		return 92;
	case XK_bracketright:
		return 93;
	case XK_grave:
		return 96;
	case XK_less:
		return 161;
	case XK_Escape:
		return 256;
	case XK_Return:
		return 257;
	case XK_Tab:
	case XK_ISO_Left_Tab:
		return 258;
	case XK_BackSpace:
		return 259;
	case XK_Insert:
		return 260;
	case XK_Delete:
		return 261;
	case XK_Right:
		return 262;
	case XK_Left:
		return 263;
	case XK_Down:
		return 264;
	case XK_Up:
		return 265;
	case XK_Page_Up:
		return 266;
	case XK_Page_Down:
		return 267;
	case XK_Home:
		return 268;
	case XK_End:
		return 269;
	case XK_Caps_Lock:
		return 280;
	case XK_Scroll_Lock:
		return 281;
	case XK_Num_Lock:
		return 282;
	case XK_Print:
		return 283;
	case XK_Pause:
		return 284;
	case XK_KP_Decimal:
	case XK_KP_Separator:
	case XK_KP_Delete:
		return 330;
	case XK_KP_Divide:
		return 331;
	case XK_KP_Multiply:
		return 332;
	case XK_KP_Subtract:
		return 333;
	case XK_KP_Add:
		return 334;
	case XK_KP_Enter:
		return 335;
	case XK_KP_Equal:
		return 336;
	case XK_KP_Insert:
		return 320;
	case XK_KP_End:
		return 321;
	case XK_KP_Down:
		return 322;
	case XK_KP_Page_Down:
		return 323;
	case XK_KP_Left:
		return 324;
	case XK_KP_Right:
		return 326;
	case XK_KP_Home:
		return 327;
	case XK_KP_Up:
		return 328;
	case XK_KP_Page_Up:
		return 329;
	case XK_Shift_L:
		return 340;
	case XK_Control_L:
		return 341;
	case XK_Alt_L:
	case XK_Meta_L:
		return 342;
	case XK_Super_L:
		return 343;
	case XK_Shift_R:
		return 344;
	case XK_Control_R:
		return 345;
	case XK_Mode_switch:
	case XK_ISO_Level3_Shift:
	case XK_Alt_R:
	case XK_Meta_R:
		return 346;
	case XK_Super_R:
		return 347;
	case XK_Menu:
		return 348;
	default:
		return 0;
	}
}

static inline int v_multiwindow_x11_key_code_from_keysyms(KeySym *keysyms, int width) {
	if (keysyms == NULL || width <= 0) {
		return 0;
	}
	if (width > 1) {
		switch (keysyms[1]) {
		case XK_KP_0:
			return 320;
		case XK_KP_1:
			return 321;
		case XK_KP_2:
			return 322;
		case XK_KP_3:
			return 323;
		case XK_KP_4:
			return 324;
		case XK_KP_5:
			return 325;
		case XK_KP_6:
			return 326;
		case XK_KP_7:
			return 327;
		case XK_KP_8:
			return 328;
		case XK_KP_9:
			return 329;
		case XK_KP_Separator:
		case XK_KP_Decimal:
			return 330;
		case XK_KP_Equal:
			return 336;
		case XK_KP_Enter:
			return 335;
		default:
			break;
		}
	}
	return v_multiwindow_x11_key_code_from_keysym(keysyms[0]);
}

static inline void v_multiwindow_x11_init_keycodes(Display *display, int *keycodes, int keycodes_len) {
	if (keycodes == NULL || keycodes_len <= 0) {
		return;
	}
	for (int i = 0; i < keycodes_len; i++) {
		keycodes[i] = 0;
	}
	if (display == NULL) {
		return;
	}

	int scancode_min = 0;
	int scancode_max = 0;
	XDisplayKeycodes(display, &scancode_min, &scancode_max);

	XkbDescPtr desc = XkbGetMap(display, 0, XkbUseCoreKbd);
	if (desc != NULL) {
		if (XkbGetNames(display, XkbKeyNamesMask | XkbKeyAliasesMask, desc) == Success && desc->names != NULL) {
			scancode_min = (int)desc->min_key_code;
			scancode_max = (int)desc->max_key_code;
			static const VMultiwindowX11KeymapEntry keymap[] = {
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(96, 'T', 'L', 'D', 'E'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(49, 'A', 'E', '0', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(50, 'A', 'E', '0', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(51, 'A', 'E', '0', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(52, 'A', 'E', '0', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(53, 'A', 'E', '0', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(54, 'A', 'E', '0', '6'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(55, 'A', 'E', '0', '7'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(56, 'A', 'E', '0', '8'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(57, 'A', 'E', '0', '9'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(48, 'A', 'E', '1', '0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(45, 'A', 'E', '1', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(61, 'A', 'E', '1', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(81, 'A', 'D', '0', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(87, 'A', 'D', '0', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(69, 'A', 'D', '0', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(82, 'A', 'D', '0', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(84, 'A', 'D', '0', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(89, 'A', 'D', '0', '6'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(85, 'A', 'D', '0', '7'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(73, 'A', 'D', '0', '8'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(79, 'A', 'D', '0', '9'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(80, 'A', 'D', '1', '0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(91, 'A', 'D', '1', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(93, 'A', 'D', '1', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(65, 'A', 'C', '0', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(83, 'A', 'C', '0', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(68, 'A', 'C', '0', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(70, 'A', 'C', '0', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(71, 'A', 'C', '0', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(72, 'A', 'C', '0', '6'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(74, 'A', 'C', '0', '7'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(75, 'A', 'C', '0', '8'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(76, 'A', 'C', '0', '9'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(59, 'A', 'C', '1', '0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(39, 'A', 'C', '1', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(90, 'A', 'B', '0', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(88, 'A', 'B', '0', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(67, 'A', 'B', '0', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(86, 'A', 'B', '0', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(66, 'A', 'B', '0', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(78, 'A', 'B', '0', '6'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(77, 'A', 'B', '0', '7'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(44, 'A', 'B', '0', '8'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(46, 'A', 'B', '0', '9'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(47, 'A', 'B', '1', '0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(92, 'B', 'K', 'S', 'L'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(161, 'L', 'S', 'G', 'T'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(32, 'S', 'P', 'C', 'E'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(256, 'E', 'S', 'C', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(257, 'R', 'T', 'R', 'N'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(258, 'T', 'A', 'B', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(259, 'B', 'K', 'S', 'P'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(260, 'I', 'N', 'S', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(261, 'D', 'E', 'L', 'E'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(262, 'R', 'G', 'H', 'T'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(263, 'L', 'E', 'F', 'T'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(264, 'D', 'O', 'W', 'N'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(265, 'U', 'P', '\0', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(266, 'P', 'G', 'U', 'P'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(267, 'P', 'G', 'D', 'N'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(268, 'H', 'O', 'M', 'E'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(269, 'E', 'N', 'D', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(280, 'C', 'A', 'P', 'S'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(281, 'S', 'C', 'L', 'K'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(282, 'N', 'M', 'L', 'K'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(283, 'P', 'R', 'S', 'C'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(284, 'P', 'A', 'U', 'S'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(290, 'F', 'K', '0', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(291, 'F', 'K', '0', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(292, 'F', 'K', '0', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(293, 'F', 'K', '0', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(294, 'F', 'K', '0', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(295, 'F', 'K', '0', '6'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(296, 'F', 'K', '0', '7'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(297, 'F', 'K', '0', '8'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(298, 'F', 'K', '0', '9'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(299, 'F', 'K', '1', '0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(300, 'F', 'K', '1', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(301, 'F', 'K', '1', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(302, 'F', 'K', '1', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(303, 'F', 'K', '1', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(304, 'F', 'K', '1', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(305, 'F', 'K', '1', '6'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(306, 'F', 'K', '1', '7'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(307, 'F', 'K', '1', '8'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(308, 'F', 'K', '1', '9'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(309, 'F', 'K', '2', '0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(310, 'F', 'K', '2', '1'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(311, 'F', 'K', '2', '2'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(312, 'F', 'K', '2', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(313, 'F', 'K', '2', '4'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(314, 'F', 'K', '2', '5'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(320, 'K', 'P', '0', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(321, 'K', 'P', '1', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(322, 'K', 'P', '2', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(323, 'K', 'P', '3', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(324, 'K', 'P', '4', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(325, 'K', 'P', '5', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(326, 'K', 'P', '6', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(327, 'K', 'P', '7', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(328, 'K', 'P', '8', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(329, 'K', 'P', '9', '\0'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(330, 'K', 'P', 'D', 'L'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(331, 'K', 'P', 'D', 'V'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(332, 'K', 'P', 'M', 'U'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(333, 'K', 'P', 'S', 'U'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(334, 'K', 'P', 'A', 'D'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(335, 'K', 'P', 'E', 'N'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(336, 'K', 'P', 'E', 'Q'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(340, 'L', 'F', 'S', 'H'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(341, 'L', 'C', 'T', 'L'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(342, 'L', 'A', 'L', 'T'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(343, 'L', 'W', 'I', 'N'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(344, 'R', 'T', 'S', 'H'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(345, 'R', 'C', 'T', 'L'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(346, 'R', 'A', 'L', 'T'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(346, 'L', 'V', 'L', '3'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(346, 'M', 'D', 'S', 'W'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(347, 'R', 'W', 'I', 'N'),
				V_MULTIWINDOW_X11_KEYMAP_ENTRY(348, 'M', 'E', 'N', 'U'),
			};
			const int keymap_len = (int)(sizeof(keymap) / sizeof(keymap[0]));
			for (int scancode = scancode_min; scancode <= scancode_max && scancode < keycodes_len; scancode++) {
				if (scancode < 0 || desc->names->keys == NULL) {
					continue;
				}
				int key = 0;
				for (int i = 0; i < keymap_len; i++) {
					if (memcmp(desc->names->keys[scancode].name, keymap[i].name, XkbKeyNameLength) == 0) {
						key = keymap[i].key;
						break;
					}
				}
				if (key == 0 && desc->names->key_aliases != NULL) {
					for (int alias_index = 0; alias_index < desc->names->num_key_aliases; alias_index++) {
						if (memcmp(desc->names->key_aliases[alias_index].real, desc->names->keys[scancode].name, XkbKeyNameLength) != 0) {
							continue;
						}
						for (int i = 0; i < keymap_len; i++) {
							if (memcmp(desc->names->key_aliases[alias_index].alias, keymap[i].name, XkbKeyNameLength) == 0) {
								key = keymap[i].key;
								break;
							}
						}
						if (key != 0) {
							break;
						}
					}
				}
				keycodes[scancode] = key;
			}
			XkbFreeNames(desc, XkbKeyNamesMask, True);
		}
		XkbFreeKeyboard(desc, 0, True);
	}

	if (scancode_min < 0) {
		scancode_min = 0;
	}
	if (scancode_max >= keycodes_len) {
		scancode_max = keycodes_len - 1;
	}
	if (scancode_min > scancode_max) {
		return;
	}
	int syms_per_code = 0;
	KeySym *keysyms = XGetKeyboardMapping(display, (KeyCode)scancode_min, scancode_max - scancode_min + 1, &syms_per_code);
	if (keysyms == NULL) {
		return;
	}
	for (int scancode = scancode_min; scancode <= scancode_max; scancode++) {
		if (keycodes[scancode] == 0) {
			int base = (scancode - scancode_min) * syms_per_code;
			keycodes[scancode] = v_multiwindow_x11_key_code_from_keysyms(keysyms + base, syms_per_code);
		}
	}
	XFree(keysyms);
}

static inline int v_multiwindow_x11_key_code(XEvent *event, int *keycodes, int keycodes_len) {
	KeySym keysym = XLookupKeysym(&event->xkey, 0);
	int keycode = v_multiwindow_x11_key_code_from_keysym(keysym);
	unsigned int scancode = event->xkey.keycode;
	if (keycode >= 256 && keycode <= 348) {
		return keycode;
	}
	if (keycodes != NULL && scancode < (unsigned int)keycodes_len && keycodes[scancode] != 0) {
		return keycodes[scancode];
	}
	return keycode;
}

static inline int v_multiwindow_x11_utf8_decode_next(const char *buf, int count, int *offset, unsigned int *out_codepoint) {
	if (buf == NULL || offset == NULL || out_codepoint == NULL || *offset >= count) {
		return 0;
	}
	const unsigned char *bytes = (const unsigned char *)buf;
	int i = *offset;
	unsigned int codepoint = 0;
	int width = 0;
	if ((bytes[i] & 0x80) == 0) {
		codepoint = bytes[i];
		width = 1;
	} else if ((bytes[i] & 0xe0) == 0xc0 && i + 1 < count) {
		codepoint = ((unsigned int)(bytes[i] & 0x1f) << 6) |
			(unsigned int)(bytes[i + 1] & 0x3f);
		width = 2;
	} else if ((bytes[i] & 0xf0) == 0xe0 && i + 2 < count) {
		codepoint = ((unsigned int)(bytes[i] & 0x0f) << 12) |
			((unsigned int)(bytes[i + 1] & 0x3f) << 6) |
			(unsigned int)(bytes[i + 2] & 0x3f);
		width = 3;
	} else if ((bytes[i] & 0xf8) == 0xf0 && i + 3 < count) {
		codepoint = ((unsigned int)(bytes[i] & 0x07) << 18) |
			((unsigned int)(bytes[i + 1] & 0x3f) << 12) |
			((unsigned int)(bytes[i + 2] & 0x3f) << 6) |
			(unsigned int)(bytes[i + 3] & 0x3f);
		width = 4;
	}
	if (width == 0 || (codepoint >= 0xd800U && codepoint <= 0xdfffU) || codepoint >= 0x110000U) {
		*offset = i + 1;
		return 0;
	}
	*offset = i + width;
	*out_codepoint = codepoint;
	return 1;
}

static inline int v_multiwindow_x11_decode_utf8_codes(const char *buf, int count, unsigned int *codes, int codes_len, int *required_codes) {
	int offset = 0;
	int out_count = 0;
	int total_count = 0;
	if (required_codes != NULL) {
		*required_codes = 0;
	}
	while (offset < count) {
		unsigned int codepoint = 0;
		if (v_multiwindow_x11_utf8_decode_next(buf, count, &offset, &codepoint) && codepoint != 0) {
			if (out_count < codes_len) {
				codes[out_count++] = codepoint;
			}
			total_count++;
		}
	}
	if (required_codes != NULL) {
		*required_codes = total_count;
	}
	return out_count;
}

static inline int v_multiwindow_x11_char_codes(XIC ic, XEvent *event, unsigned int *codes, int codes_len, int *required_codes) {
	if (required_codes != NULL) {
		*required_codes = 0;
	}
	if (ic == NULL || event == NULL || event->type != KeyPress || codes == NULL || codes_len <= 0) {
		return 0;
	}
	char stack_buf[V_MULTIWINDOW_X11_XIM_STACK_BYTES];
	char *buf = stack_buf;
	int buf_len = (int)sizeof(stack_buf);
	KeySym keysym = NoSymbol;
	Status status = 0;
	int count = Xutf8LookupString(ic, &event->xkey, buf, buf_len, &keysym, &status);
	if (status == XBufferOverflow) {
		if (count <= 0 || count > V_MULTIWINDOW_X11_XIM_MAX_BYTES) {
			return 0;
		}
		buf = (char *)malloc((size_t)count);
		if (buf == NULL) {
			return 0;
		}
		buf_len = count;
		status = 0;
		count = Xutf8LookupString(ic, &event->xkey, buf, buf_len, &keysym, &status);
	}
	if ((status != XLookupChars && status != XLookupBoth) || count <= 0 || count > buf_len) {
		if (buf != stack_buf) {
			free(buf);
		}
		return 0;
	}
	int out_count = v_multiwindow_x11_decode_utf8_codes(buf, count, codes, codes_len, required_codes);
	if (buf != stack_buf) {
		free(buf);
	}
	return out_count;
}

static inline int v_multiwindow_x11_apply_config_hints(Display *display, unsigned long window, int width, int height, int min_width, int min_height, int resizable, int borderless, int fullscreen) {
	XSizeHints size_hints;
	memset(&size_hints, 0, sizeof(size_hints));
	size_hints.flags = PSize;
	size_hints.width = width;
	size_hints.height = height;
	if (min_width > 0 || min_height > 0) {
		size_hints.flags |= PMinSize;
		size_hints.min_width = min_width > 0 ? min_width : 1;
		size_hints.min_height = min_height > 0 ? min_height : 1;
	}
	if (!resizable) {
		size_hints.flags |= PMinSize | PMaxSize;
		size_hints.min_width = width;
		size_hints.min_height = height;
		size_hints.max_width = width;
		size_hints.max_height = height;
	}
	XSetWMNormalHints(display, (Window)window, &size_hints);

	if (borderless) {
		Atom motif_hints_atom = XInternAtom(display, "_MOTIF_WM_HINTS", False);
		if (motif_hints_atom == None) {
			return 0;
		}
		VMultiwindowMotifWmHints motif_hints;
		memset(&motif_hints, 0, sizeof(motif_hints));
		motif_hints.flags = MWM_HINTS_DECORATIONS;
		motif_hints.decorations = 0;
		XChangeProperty(display, (Window)window, motif_hints_atom, motif_hints_atom, 32, PropModeReplace, (unsigned char *)&motif_hints, 5);
	}

	if (fullscreen) {
		Atom state_atom = XInternAtom(display, "_NET_WM_STATE", False);
		Atom fullscreen_atom = XInternAtom(display, "_NET_WM_STATE_FULLSCREEN", False);
		if (state_atom == None || fullscreen_atom == None) {
			return 0;
		}
		XChangeProperty(display, (Window)window, state_atom, XA_ATOM, 32, PropModeReplace, (unsigned char *)&fullscreen_atom, 1);
	}

	return 1;
}

static inline int v_multiwindow_x11_get_window_size(Display *display, unsigned long window, int *out_width, int *out_height) {
	XWindowAttributes attrs;
	memset(&attrs, 0, sizeof(attrs));
	if (!XGetWindowAttributes(display, (Window)window, &attrs)) {
		return 0;
	}
	if (attrs.width <= 0 || attrs.height <= 0) {
		return 0;
	}
	*out_width = attrs.width;
	*out_height = attrs.height;
	return 1;
}

static inline void *v_multiwindow_x11_egl_get_display(Display *display) {
	return (void *)eglGetDisplay((EGLNativeDisplayType)display);
}

static inline int v_multiwindow_x11_egl_initialize(void *egl_display) {
	return eglInitialize((EGLDisplay)egl_display, NULL, NULL) == EGL_TRUE ? 1 : 0;
}

static inline int v_multiwindow_x11_egl_bind_opengl_api(void) {
	return eglBindAPI(EGL_OPENGL_API) == EGL_TRUE ? 1 : 0;
}

static inline int v_multiwindow_x11_egl_choose_config(void *egl_display, void **out_config, int *out_visual_id) {
	const EGLint attrs[] = {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
		EGL_RED_SIZE, 8,
		EGL_GREEN_SIZE, 8,
		EGL_BLUE_SIZE, 8,
		EGL_ALPHA_SIZE, 8,
		EGL_DEPTH_SIZE, 24,
		EGL_STENCIL_SIZE, 8,
		EGL_NONE
	};
	EGLConfig config = NULL;
	EGLint config_count = 0;
	EGLint visual_id = 0;
	if (eglChooseConfig((EGLDisplay)egl_display, attrs, &config, 1, &config_count) != EGL_TRUE || config_count == 0) {
		return 0;
	}
	if (eglGetConfigAttrib((EGLDisplay)egl_display, config, EGL_NATIVE_VISUAL_ID, &visual_id) != EGL_TRUE || visual_id == 0) {
		return 0;
	}
	*out_config = (void *)config;
	*out_visual_id = (int)visual_id;
	return 1;
}

static inline void *v_multiwindow_x11_egl_create_context(void *egl_display, void *egl_config) {
	const EGLint core_attrs[] = {
		EGL_CONTEXT_MAJOR_VERSION, 3,
		EGL_CONTEXT_MINOR_VERSION, 3,
		EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
		EGL_NONE
	};
	EGLContext context = eglCreateContext((EGLDisplay)egl_display, (EGLConfig)egl_config, EGL_NO_CONTEXT, core_attrs);
	if (context == EGL_NO_CONTEXT) {
		const EGLint fallback_attrs[] = { EGL_NONE };
		context = eglCreateContext((EGLDisplay)egl_display, (EGLConfig)egl_config, EGL_NO_CONTEXT, fallback_attrs);
	}
	return context == EGL_NO_CONTEXT ? NULL : (void *)context;
}

static inline unsigned long v_multiwindow_x11_create_egl_window(Display *display, unsigned long root, int screen, int native_visual_id, int width, int height, unsigned long *out_colormap) {
	XVisualInfo template_info;
	XVisualInfo *visual_info = NULL;
	XSetWindowAttributes attrs;
	int visual_count = 0;
	Window window = 0;

	template_info.visualid = (VisualID)native_visual_id;
	template_info.screen = screen;
	visual_info = XGetVisualInfo(display, VisualIDMask | VisualScreenMask, &template_info, &visual_count);
	if (visual_info == NULL || visual_count <= 0) {
		return 0;
	}

	memset(&attrs, 0, sizeof(attrs));
	attrs.colormap = XCreateColormap(display, root, visual_info->visual, AllocNone);
	attrs.border_pixel = 0;
	attrs.background_pixel = 0;
	attrs.event_mask = v_multiwindow_x11_event_mask();
	window = XCreateWindow(display, root, 0, 0, (unsigned int)width, (unsigned int)height, 0,
		visual_info->depth, InputOutput, visual_info->visual,
		CWColormap | CWBorderPixel | CWBackPixel | CWEventMask, &attrs);
	if (window == 0) {
		XFreeColormap(display, attrs.colormap);
		attrs.colormap = 0;
	}
	if (out_colormap != NULL) {
		*out_colormap = attrs.colormap;
	}
	XFree(visual_info);
	return window;
}

static inline void *v_multiwindow_x11_egl_create_window_surface(void *egl_display, void *egl_config, unsigned long window) {
	EGLSurface surface = eglCreateWindowSurface((EGLDisplay)egl_display, (EGLConfig)egl_config, (EGLNativeWindowType)window, NULL);
	return surface == EGL_NO_SURFACE ? NULL : (void *)surface;
}

static inline int v_multiwindow_x11_egl_make_current(void *egl_display, void *egl_surface, void *egl_context) {
	return eglMakeCurrent((EGLDisplay)egl_display, (EGLSurface)egl_surface, (EGLSurface)egl_surface, (EGLContext)egl_context) == EGL_TRUE ? 1 : 0;
}

static inline void v_multiwindow_x11_egl_clear_current(void *egl_display) {
	eglMakeCurrent((EGLDisplay)egl_display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
}

static inline int v_multiwindow_x11_egl_swap_buffers(void *egl_display, void *egl_surface) {
	return eglSwapBuffers((EGLDisplay)egl_display, (EGLSurface)egl_surface) == EGL_TRUE ? 1 : 0;
}

static inline void v_multiwindow_x11_egl_destroy_surface(void *egl_display, void *egl_surface) {
	if (egl_surface != NULL) {
		eglDestroySurface((EGLDisplay)egl_display, (EGLSurface)egl_surface);
	}
}

static inline void v_multiwindow_x11_egl_destroy_context(void *egl_display, void *egl_context) {
	if (egl_context != NULL) {
		eglDestroyContext((EGLDisplay)egl_display, (EGLContext)egl_context);
	}
}

static inline void v_multiwindow_x11_egl_terminate(void *egl_display) {
	if (egl_display != NULL) {
		eglTerminate((EGLDisplay)egl_display);
	}
}

#endif
