module gg

@[if gg_record ?]
pub fn (mut ctx Context) record_frame() {}

@[if gg_record ?]
fn (mut ctx Context) stop_recording_if_needed() {}
