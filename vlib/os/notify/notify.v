module notify

import time

// Not strictly necessary but provides a good template when writing the backends
interface FdNotifier {
	add(fd int, events FdEventType, conf ...FdConfigFlags) ?
	remove(fd int) ?
	wait(timeout time.Duration) []FdEvent
	close() ?
}

interface FdEvent {
	fd int
	kind FdEventType
}

[flag]
pub enum FdEventType {
	read
	write
	peer_hangup
	exception
	error
	hangup
}

[flag]
pub enum FdConfigFlags {
	edge_trigger
	one_shot
	wake_up
	exclusive
}
