// BEAM Backend: Coroutines map naturally to Erlang/OTP lightweight processes.
// On BEAM, V's coroutine model maps to Erlang processes:
//   - spawn() maps to erlang:spawn/1 (already sub-microsecond, no OS threads)
//   - sleep() maps to timer:sleep/1 or receive-after timeout
//   - Channels map to Erlang message passing (send ! receive)
//
// The BEAM VM inherently provides millions of concurrent lightweight processes
// with preemptive scheduling, making explicit coroutine support unnecessary.
// This module provides API compatibility with V's coroutine interface.
module coroutines

import time

// sleep is coroutine-safe version of time.sleep().
// BEAM: delegates to time.sleep which maps to timer:sleep/1 in codegen.
// On BEAM, all processes are cooperatively scheduled by the VM, so any sleep
// automatically yields the process scheduler without blocking other processes.
pub fn sleep(duration time.Duration) {
	time.sleep(duration)
}
