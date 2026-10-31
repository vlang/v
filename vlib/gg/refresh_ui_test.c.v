// vtest build: !docker-ubuntu-musl
module gg

struct RefreshUiTestState {
mut:
	ctx              &Context = unsafe { nil }
	frames           int
	refresh_in_frame bool
}

fn refresh_ui_test_frame(mut state RefreshUiTestState) {
	state.frames++
	if state.refresh_in_frame {
		state.refresh_in_frame = false
		request := spawn refresh_ui_test_request(mut state.ctx)
		request.wait()
	}
}

fn refresh_ui_test_request(mut ctx Context) {
	ctx.refresh_ui()
}

fn refresh_ui_test_context(mut state RefreshUiTestState) &Context {
	mut ctx := &Context{
		ui_mode:   true
		user_data: state
		config:    Config{
			frame_fn:      refresh_ui_test_frame
			swap_interval: 0
		}
	}
	state.ctx = ctx
	return ctx
}

fn test_refresh_ui_restarts_trailing_frames_on_the_frame_thread() {
	mut state := &RefreshUiTestState{}
	mut ctx := refresh_ui_test_context(mut state)
	for _ in 0 .. 5 {
		gg_frame_fn(mut ctx)
	}
	assert state.frames == 4
	assert ctx.ticks == 4
	ctx.refresh_ui()
	assert ctx.ticks == 4
	gg_frame_fn(mut ctx)
	assert state.frames == 5
	assert ctx.ticks == 0
}

fn test_refresh_ui_during_a_frame_is_preserved_for_the_next_frame() {
	mut state := &RefreshUiTestState{
		refresh_in_frame: true
	}
	mut ctx := refresh_ui_test_context(mut state)
	gg_frame_fn(mut ctx)
	gg_frame_fn(mut ctx)
	assert state.frames == 2
	assert ctx.ticks == 0
	for _ in 0 .. 4 {
		gg_frame_fn(mut ctx)
	}
	assert state.frames == 5
}
