module canvas

// A simulation of the parts of Skia Graphite's DrawBufferManager, DrawWriter
// and UniformTracker that decide where a sorted draw list is split into draw
// calls. Graphite appends instance data and storage-buffer uniforms into
// growing buffer blocks; whenever a block is full or the uniform binding
// changes, the pending instances are issued as their own instanced draw.
// Splitting a batch changes MSAA results, so these boundaries are reproduced
// exactly. Only offsets and sizes are tracked, never the data.

// Metal on macOS: 256-byte uniform/storage binding alignment, 4-byte vertex
// buffer alignment, and directly mapped draw buffers (no transfer buffers).
const storage_min_alignment = u32(256)
const vertex_min_alignment = u32(4)

struct SubAllocator {
mut:
	buffer    int // 0 = no buffer
	size      u32
	offset    u32
	stride    u32 = 1
	remaining u32
}

fn (a &SubAllocator) remaining_bytes() u32 {
	return if a.buffer != 0 { a.size - a.offset } else { 0 }
}

fn (mut a SubAllocator) reset_for_new_binding() {
	a.remaining = 0
	a.stride = 0
}

fn lcm_alignment(maybe_pow2 u32, prob_non_pow2 u32) u32 {
	if maybe_pow2 == 1 || maybe_pow2 == prob_non_pow2 || (maybe_pow2 & (maybe_pow2 - 1) == 0
		&& prob_non_pow2 > maybe_pow2 && prob_non_pow2 & (maybe_pow2 - 1) == 0) {
		return prob_non_pow2
	}
	mut a := maybe_pow2
	mut b := prob_non_pow2
	for b != 0 {
		t := a % b
		a = b
		b = t
	}
	return maybe_pow2 / a * prob_non_pow2
}

@[inline]
fn align_non_pow2(x u32, alignment u32) u32 {
	return ((x + alignment - 1) / alignment) * alignment
}

// prep_for_stride mirrors BufferSubAllocator::prepForStride().
fn (mut a SubAllocator) prep_for_stride(stride u32, align u32, min_count u32, headroom u32, min_alignment u32) {
	if a.buffer != 0 {
		if a.stride == stride && (align == 1 || align == stride) {
			return
		}
		mut align32 := lcm_alignment(align, stride)
		if a.stride == 0 {
			align32 = lcm_alignment(min_alignment, align32)
		}
		reserve_for_headroom := if headroom > stride { headroom - stride } else { u32(0) }
		if a.remaining_bytes() >= align32 - 1 + reserve_for_headroom {
			offset := align_non_pow2(a.offset, align32)
			a.stride = stride
			a.remaining = (a.size - offset - reserve_for_headroom) / a.stride
			if a.remaining > 0 && a.remaining >= min_count {
				a.offset = offset
				return
			}
		}
	}
	a.remaining = 0
}

// reserve mirrors BufferSubAllocator::reserve(); it returns false when the
// allocator cannot hold `count` more blocks.
fn (mut a SubAllocator) reserve(count u32) bool {
	if a.buffer == 0 || count > a.remaining {
		return false
	}
	a.offset += count * a.stride
	a.remaining -= count
	return true
}

struct BufferState {
	min_alignment u32
	min_block     u32
	max_block     u32
mut:
	last_size u32
	available SubAllocator
}

fn new_buffer_state(min_alignment u32, min_size u32, max_size u32) BufferState {
	return BufferState{
		min_alignment: min_alignment
		min_block:     align_pow2(min_size, min_alignment)
		max_block:     align_pow2(max_size, min_alignment)
	}
}

@[inline]
fn align_pow2(x u32, alignment u32) u32 {
	return (x + alignment - 1) & ~(alignment - 1)
}

const vertex_state = 0
const storage_state = 1

// BufferManagerSim mirrors the vertex and storage states of a Recorder's
// DrawBufferManager. Graphite resets the growth state when a recording is
// snapped, which happens once per surface flush.
struct BufferManagerSim {
mut:
	states      [2]BufferState
	next_buffer int = 1
}

fn new_buffer_manager_sim() BufferManagerSim {
	return BufferManagerSim{
		states: [new_buffer_state(vertex_min_alignment, 16 << 10, 1 << 20),
			new_buffer_state(storage_min_alignment, 2 << 10, 1 << 20)]!
	}
}

// get_buffer mirrors DrawBufferManager::getBuffer() for mapped, non-shareable buffers.
fn (mut m BufferManagerSim) get_buffer(index int, count u32, stride u32, xtra_alignment u32, headroom u32) SubAllocator {
	mut state := &m.states[index]
	required := if headroom > count * stride { headroom } else { count * stride }
	state.available.reset_for_new_binding()
	state.available.prep_for_stride(stride, xtra_alignment, count, headroom, state.min_alignment)
	if state.available.remaining >= count {
		result := state.available
		state.available = SubAllocator{}
		return result
	}
	m.retire(index, mut state.available)
	mut size := align_pow2(required, state.min_block)
	if size < state.max_block {
		doubled := if state.last_size * 2 < state.max_block {
			state.last_size * 2
		} else {
			state.max_block
		}
		if doubled > size {
			size = doubled
		}
		state.last_size = size
	} else {
		state.last_size = state.max_block
	}
	buffer := m.next_buffer
	m.next_buffer++
	return SubAllocator{
		buffer:    buffer
		size:      size
		offset:    0
		stride:    stride
		remaining: if stride > 0 && stride <= size { size / stride } else { u32(0) }
	}
}

// retire mirrors BufferSubAllocator::reset(): a buffer with more room left
// than the manager's available one is kept for later allocations.
fn (mut m BufferManagerSim) retire(index int, mut a SubAllocator) {
	if a.buffer == 0 {
		return
	}
	mut state := &m.states[index]
	if state.available.buffer == a.buffer || a.remaining_bytes() < state.available.remaining_bytes()
		|| a.remaining_bytes() < state.min_alignment {
		// Returned to the manager without further suballocation.
	} else {
		previous := state.available
		state.available = a
		if previous.buffer != 0 && previous.buffer != a.buffer {
			// The previous available buffer goes through the first branch.
		}
	}
	a = SubAllocator{}
}

// get_mapped_buffer mirrors DrawBufferManager::getMappedBuffer(); it returns
// the suballocator after reserving `count` blocks at the aligned offset.
fn (mut m BufferManagerSim) get_mapped_buffer(index int, count u32, stride u32, reserved_count u32, xtra_alignment u32) SubAllocator {
	mut a := m.get_buffer(index, if count > reserved_count { count } else { reserved_count },
		stride, xtra_alignment, 0)
	a.prep_for_stride(stride, 1, count, 0, m.states[index].min_alignment)
	a.reserve(count)
	return a
}

// DrawWriterSim mirrors DrawWriter for instanced render steps. `flushes`
// counts issued draw calls; a draw call boundary occurs when it increments.
struct DrawWriterSim {
mut:
	current SubAllocator
	stride  u32
	pending u32
	// Boundaries in units of appended instances, in sorted order.
	calls []int
	total int
}

fn (mut w DrawWriterSim) flush() {
	if w.pending == 0 {
		return
	}
	w.calls << w.total
	w.pending = 0
}

fn (mut w DrawWriterSim) new_pipeline_state(stride u32, mut m BufferManagerSim) {
	w.flush()
	w.stride = stride
	w.current.prep_for_stride(stride, stride, 0, 0, m.states[vertex_state].min_alignment)
	w.current.reserve(0)
}

fn (mut w DrawWriterSim) append_instance(mut m BufferManagerSim) {
	if 1 > w.current.remaining {
		// realloc(): flush, then allocate at least one stride-aligned block.
		w.flush()
		mut next := m.get_mapped_buffer(vertex_state, 0, w.stride, 1, w.stride)
		m.retire(vertex_state, mut w.current)
		w.current = next
	}
	w.current.reserve(1)
	w.pending++
	w.total++
}

// UniformTrackerSim mirrors DrawListBase::UniformTracker with storage buffers.
struct UniformTrackerSim {
mut:
	current      SubAllocator
	last_index   int = -1
	last_buffer  int
	entry_buffer map[int]int // uniform index -> buffer holding its data
}

// write_uniforms returns whether the uniform binding changed.
fn (mut t UniformTrackerSim) write_uniforms(index int, size u32, mut m BufferManagerSim) bool {
	if index < 0 || index == t.last_index {
		return false
	}
	t.last_index = index
	mut buffer := t.entry_buffer[index] or { 0 }
	if buffer == 0 || buffer != t.last_buffer {
		t.current.prep_for_stride(size, 1, 1, size, m.states[storage_state].min_alignment)
		if t.current.reserve(1) {
			buffer = t.current.buffer
		} else {
			mut next := m.get_mapped_buffer(storage_state, 1, size, 0, 1)
			m.retire(storage_state, mut t.current)
			t.current = next
			buffer = t.current.buffer
		}
		t.entry_buffer[index] = buffer
	}
	rebind := buffer != t.last_buffer
	t.last_buffer = buffer
	return rebind
}
