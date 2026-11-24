The files in this directory implement V's captured-function runtime, called internally by the
compiler.

Linux amd64/arm64 and macOS C-backend freestanding programs support captured functions. Linux maps
separate writable metadata and executable thunk pages through its raw mapping layer. Instructions
are flushed from the instruction cache and pages become read/execute before a callback is published.
Allocating another page never makes existing executable pages writable. ARM64 Linux uses the actual
kernel page size, including 64 KiB pages, when encoding the displacement to callback metadata.

On macOS, freestanding programs remap immutable trampoline templates from the executable's
signed text beside writable metadata using Mach VM APIs. The trampoline code is never writable, and
this path needs no JIT entitlement on Intel Macs or Apple silicon. On amd64 the freestanding
runtime issues Mach requests directly and uses kernel-backed locks and thread identities.
The arm64 macOS path continues to use libSystem.
The freestanding runtime uses real locks and thread identities for callback allocation and lifetime
tracking. Hosted paths retain their existing behavior.

Releasing callbacks returns their slots to the runtime's pool; executable pages remain mapped for
reuse at the process's high-water mark. A callback must not execute after its lifetime is reclaimed,
or while another thread releases that same callback. These are the existing callback ownership
rules.
