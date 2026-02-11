// BEAM backend stub for fasthttp module
// Provides type-compatible placeholders so V code compiles on BEAM.
// The main fasthttp.v defines Slice, HttpRequest, HttpResponse, ServerConfig as plain .v types.
// The platform-specific files (fasthttp_linux.v, fasthttp_darwin.v, fasthttp_windows.v)
// define Server and the run() method using C-level I/O (epoll, kqueue, etc.).
// On BEAM: Those platform .v files are still compiled (they are OS selectors, not backend selectors),
// so Server is already defined there. We do NOT redefine it here.
module fasthttp
