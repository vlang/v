# goroutines

Go-style goroutine runtime for V, implementing the GMP (Goroutine-Machine-Processor) scheduling model translated from the Go runtime (`src/runtime/proc.go`, `runtime2.go`, `chan.go`).

## Overview

This module provides lightweight goroutines for V's `go` keyword, as opposed to `spawn` which creates OS threads.

### GMP Model

- **G (Goroutine)**: Lightweight unit of execution with its own stack (~8KB default)
- **M (Machine)**: OS thread that executes goroutines
- **P (Processor)**: Logical processor with a local run queue (one per CPU core)

### Key Features

- **Work stealing**: Idle processors steal work from busy ones
- **Local run queues**: Lock-free per-P queues minimize contention
- **Global run queue**: Overflow and fairness mechanism
- **Goroutine parking**: Efficient blocking/unblocking for channels
- **G reuse**: Dead goroutines are recycled to reduce allocation

## Usage

```v
// `go` launches a goroutine (lightweight, scheduled by GMP)
go expensive_computation()

// `spawn` launches an OS thread (traditional V behavior)
spawn blocking_io_task()
```

## Architecture

Translated from Go's runtime source:

| Go Source | V Module File | Purpose |
|-----------|---------------|---------|
| `runtime2.go` | `goroutines.v` | Core data structures (G, M, P, Sched) |
| `proc.go` | `scheduler.v` | Scheduler loop, work stealing, run queues |
| `proc.go` | `park.v` | gopark/goready, Sudog, WaitQ |
| `proc.go` | `init.v` | Initialization (schedinit, procresize) |
| `chan.go` | `chan.v` | Channel implementation |
| asm (gogo/gosave) | `context_nix.c.v` | Context switching (ucontext) |
| asm (gogo/gosave) | `context_windows.c.v` | Context switching (Windows fibers) |

## References

- [Go Scheduler Design Doc](https://golang.org/s/go11sched)
- [Go Runtime Source](https://github.com/golang/go/tree/master/src/runtime)
