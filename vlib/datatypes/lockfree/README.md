# Lockfree Library for V

A high-performance, thread-safe collection of lock-free data structures for 
the V programming language. Designed for concurrent applications requiring 
low-latency and high-throughput data processing.

## Features

- **Truly Lock-Free**: No mutexes or spinlocks
- **Cross-Platform**: Works on Windows, Linux, macOS
- **Configurable**: Tune parameters for specific workloads
- **High Performance**: Optimized for modern multi-core processors

## Data Structures

### 1. Atomic Counter

A thread-safe counter with atomic operations.

```v
import datatypes.lockfree

mut counter := lockfree.new_counter[int](0)
counter.increment()
counter.increment_by(5)
value := counter.get() // 6
counter.decrement()
counter.clear()
```

**Features**:
- Atomic increment/decrement
- Batch operations
- Get current value
- Reset functionality

### 2. Ring Buffer

A circular buffer for producer-consumer scenarios.

```v
import datatypes.lockfree

mut rb := lockfree.new_ringbuffer[int](1024)
rb.push(10)
rb.push(20)
item := rb.pop() // 10
free := rb.remaining()
```

**Features**:
- Single/Multi producer/consumer modes
- Blocking/non-blocking operations
- Batch operations
- Configurable size
- Memory efficient

## Acknowledgements

This library incorporates research and design principles from:
- Intel Threading Building Blocks (TBB)
- Facebook Folly
- Java Concurrent Package
- Dmitry Vyukov's lock-free algorithms
- DPDK rte ring

---
