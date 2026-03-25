// vtest build: !docker-ubuntu-musl // needs GL/gl.h
module gg

import time

fn test_swap_interval_frame_budget() {
	assert swap_interval_frame_budget(-1) == time.Duration(0)
	assert swap_interval_frame_budget(0) == time.Duration(0)
	assert swap_interval_frame_budget(1) == time.second / 60
	assert swap_interval_frame_budget(2) == time.second / 30
	assert swap_interval_frame_budget(3) == time.second / 20
}
