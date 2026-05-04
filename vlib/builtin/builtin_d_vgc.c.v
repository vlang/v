// builtin_d_vgc.c.v - V Garbage Collector additional support
// Conditionally compiled when -d vgc is used.
// The gc_heap_usage/gc_memory_use functions are in allocation.c.v with $if vgc ? branches.

module builtin

// This file exists to ensure the vgc compile define activates VGC-specific code paths
// throughout the builtin module. The actual VGC implementation is in:
//   - vgc_d_vgc.c.v: Core types, heap, allocation
//   - vgc_gc_d_vgc.c.v: GC mark, sweep, orchestration
