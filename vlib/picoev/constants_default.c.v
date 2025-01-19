module picoev

// max_fds is the maximum number of file descriptors that can be managed.
// Many sizes depend on it, and some internal arrays are also iterated based on it,
// so increasing it a lot can slow down looping :-| .
pub const max_fds = 1024
