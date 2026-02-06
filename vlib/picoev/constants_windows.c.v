module picoev

// max_fds is the maximum number of file descriptors that can be managed.
// Many sizes depend on it, and some internal arrays are also iterated based on it,
// so increasing it a lot can slow down looping :-| .
// It is higher on windows, because if you start a veb/picoev webservice in a thread,
// the returned file descriptors can be higher than 1024 in value, especially if you
// also have a webview running in another thread, that also opens its own file descriptors.
// Note: this works, because on windows we use select, and select on win32,
// is not limited to polling on only 1024 fds.
pub const max_fds = 4096
