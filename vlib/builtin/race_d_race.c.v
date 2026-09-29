module builtin

// Programs built with `v -race` link the ThreadSanitizer runtime. race_options.c provides
// its default options, and reads VRACE, V's counterpart of Go's GORACE.
#flag @VEXEROOT/vlib/builtin/race_options.c
