module builtin

// provide an empty function when manual memory management is used
// to simplify leak detection
//
fn gcboehm_check_leaks() {}
