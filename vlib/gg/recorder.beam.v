// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// Frame recording is a no-op on BEAM since there is no GPU rendering pipeline.
module gg

fn (mut ctx Context) record_frame_impl() {
}
