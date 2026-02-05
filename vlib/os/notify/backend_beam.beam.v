// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module notify

import time

// BEAM backend for FdNotifier
// On BEAM: Would use gen_tcp:controlling_process or similar mechanisms

// BeamFdNotifier is the BEAM implementation of FdNotifier
struct BeamFdNotifier {
mut:
	fds []int
}

// new creates a new FdNotifier for the BEAM backend
pub fn new() !FdNotifier {
	return &BeamFdNotifier{
		fds: []int{}
	}
}

// add registers the file descriptor `fd` for notifications
pub fn (mut n BeamFdNotifier) add(fd int, events FdEventType, conf ...FdConfigFlags) ! {
	n.fds << fd
}

// modify modifies the registration for fd
pub fn (mut n BeamFdNotifier) modify(fd int, events FdEventType, conf ...FdConfigFlags) ! {
	// Placeholder
}

// remove removes the registration for fd
pub fn (mut n BeamFdNotifier) remove(fd int) ! {
	n.fds = n.fds.filter(it != fd)
}

// wait waits for events with a timeout
pub fn (n BeamFdNotifier) wait(timeout time.Duration) []FdEvent {
	// Placeholder - would use Erlang's gen_tcp/gen_udp or polling mechanisms
	return []FdEvent{}
}

// close closes the notifier
pub fn (mut n BeamFdNotifier) close() ! {
	n.fds.clear()
}
