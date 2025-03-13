module gg

// is_key_down returns whether the given key is currently pressed.
// You can use this, if you do not want to implement your own key event handling.
@[inline]
pub fn (mut ctx Context) is_key_down(k KeyCode) bool {
	return ctx.pressed_keys[int(k)]
}
