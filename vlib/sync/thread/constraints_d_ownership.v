module thread

// Transferable constrains a generic parameter to values that can move to another thread.
pub struct Transferable {}

// Shareable constrains a generic parameter to values that can be shared across threads
// and destroyed on a different thread.
pub struct Shareable {}
