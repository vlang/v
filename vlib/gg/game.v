// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// FixedStep converts variable frame times into a stable number of simulation
// steps. It clamps runaway accumulators so a long stall cannot freeze a game.
pub struct FixedStep {
pub mut:
	step        f32 = 1.0 / 60
	accumulator f32
	max_steps   int = 5
}

// new_fixed_step returns a fixed-step accumulator with the given step size.
pub fn new_fixed_step(step f32) FixedStep {
	return FixedStep{ step: step }
}

// advance consumes dt and returns how many fixed steps should be simulated.
pub fn (mut f FixedStep) advance(dt f32) int {
	if f.step <= 0 {
		f.accumulator = 0
		return 0
	}
	f.accumulator += dt
	mut steps := 0
	for f.accumulator >= f.step && steps < f.max_steps {
		f.accumulator -= f.step
		steps++
	}
	if steps == f.max_steps && f.accumulator >= f.step {
		f.accumulator = 0
	}
	return steps
}

// FrameStats tracks a smoothed-enough FPS estimate over half-second windows.
pub struct FrameStats {
pub mut:
	fps     f32
	frames  int
	elapsed f32
	window  f32
}

// update records one frame of dt and refreshes fps roughly twice a second.
pub fn (mut s FrameStats) update(dt f32) {
	s.frames++
	s.elapsed += dt
	s.window += dt
	if s.window >= 0.5 {
		s.fps = f32(s.frames) / s.window
		s.frames = 0
		s.window = 0
	}
}

// GameState is a named state with an update callback.
pub struct GameState {
pub:
	name   string
	update fn (dt f32) @[required]
}

// StateMachine runs exactly one named state's update callback per frame.
pub struct StateMachine {
mut:
	states map[string]GameState
pub mut:
	current string
	updates int
}

// new_state_machine returns an empty state machine.
pub fn new_state_machine() StateMachine {
	return StateMachine{ states: map[string]GameState{} }
}

// add registers a state. Re-adding a name replaces its callback.
pub fn (mut sm StateMachine) add(name string, update fn (dt f32)) {
	sm.states[name] = GameState{ name: name, update: update }
}

// set switches to a registered state. Unknown names are ignored.
pub fn (mut sm StateMachine) set(name string) {
	if name !in sm.states {
		return
	}
	sm.current = name
}

// update runs the current state's callback, if one is selected.
pub fn (mut sm StateMachine) update(dt f32) {
	if sm.current == '' {
		return
	}
	state := sm.states[sm.current] or { return }
	state.update(dt)
	sm.updates++
}
