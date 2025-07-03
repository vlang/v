# Lockfree Library for V

A high-performance, thread-safe collection of lock-free data structures for 
the V programming language. Designed for concurrent applications requiring 
low-latency and high-throughput data processing.

## Features

- **Truly Lock-Free**: No mutexes or spinlocks
- **Cross-Platform**: Works on Windows, Linux, macOS
- **Memory Safe**: Built-in hazard pointers and safe memory reclamation
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

### 2. Lock-Free Stack (LIFO)(TBD)

A Last-In-First-Out stack with push/pop operations.

```v ignore
import datatypes.lockfree

mut stack := lockfree.new_stack[int]()
stack.push(42)
stack.push(100)
top := stack.pop() // 100
size := stack.size()
```

**Features**:
- Push/pop operations
- Batch push/pop
- Size approximation
- Empty check
- Thread-safe iteration

### 3. Ring Buffer

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

### 4. Double-Ended Queue (Deque)(TBD)

A thread-safe double-ended queue.

```v ignore
import datatypes.lockfree

mut deque := lockfree.new_deque[string]()
deque.push_front("first")
deque.push_back("last")
front := deque.pop_front() // "first"
back := deque.pop_back() // "last"
```

**Features**:
- Push/pop from both ends
- Size tracking
- Empty check
- Batch operations
- Iterators

### 5. Concurrent Map(TBD)

A thread-safe key-value store.

```v ignore
import datatypes.lockfree

mut cmap := lockfree.new_map[string, int]()
cmap.set("answer", 42)
value := cmap.get("answer") // 42
exists := cmap.exists("answer")
cmap.delete("answer")
```

**Features**:
- Get/set/delete operations
- Key existence check
- Size approximation
- Iterators
- Atomic updates

## Best Practices

1. **Choose the right mode**:
   - SPSC: Single Producer Single Consumer
   - MPSC: Multiple Producer Single Consumer
   - MPMC: Multiple Producer Multiple Consumer

2. **Batch operations**:
   ```v ignore
   // More efficient than single pushes
   stack.push_batch([1, 2, 3, 4, 5])
   ```

3. **Monitor contention**:
   ```v ignore
   stats := stack.stats()
   if stats.avg_retries > 5.0 {
       // Consider tuning configuration
   }
   ```

4. **Use appropriate backoff**:
   - Low contention: Minimal backoff
   - High contention: Exponential backoff

## Acknowledgements

This library incorporates research and design principles from:
- Intel Threading Building Blocks (TBB)
- Facebook Folly
- Java Concurrent Package
- Dmitry Vyukov's lock-free algorithms
- DPDK rte ring

---

**Lockfree Library** - Build high-performance concurrent applications with confidence.

