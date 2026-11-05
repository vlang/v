@[has_globals]
module gg

$if test {
	import gg.render

	// The observer remains deliberately independent from the materialization cache.
	const multiwindow_sgl_load_proof_capacity = 16

	struct MultiWindowSglLoadProofRecord {
		generation  u64
		sequence    u64
		pipeline_id int
		window      WindowId
		context_id  u64
		target_key  string
	}

	struct MultiWindowSglLoadProofSnapshot {
		generation u64
		armed      bool
		overflow   bool
		records    []MultiWindowSglLoadProofRecord
	}

	struct MultiWindowSglLoadProofState {
	mut:
		next_generation u64 = 1
		generation      u64
		armed           bool
		overflow        bool
		count           int
		records         [multiwindow_sgl_load_proof_capacity]MultiWindowSglLoadProofRecord
	}

	__global multiwindow_sgl_load_proof = MultiWindowSglLoadProofState{}

	fn multiwindow_sgl_load_proof_arm_for_test() !u64 {
		if multiwindow_sgl_load_proof.armed || multiwindow_sgl_load_proof.next_generation == 0 {
			return error('gg.multiwindow: SGL load proof is already armed or exhausted')
		}
		generation := multiwindow_sgl_load_proof.next_generation
		multiwindow_sgl_load_proof.next_generation = if generation == ~u64(0) {
			u64(0)
		} else {
			generation + 1
		}
		multiwindow_sgl_load_proof.generation = generation
		multiwindow_sgl_load_proof.armed = true
		multiwindow_sgl_load_proof.overflow = false
		multiwindow_sgl_load_proof.count = 0
		for index in 0 .. multiwindow_sgl_load_proof_capacity {
			multiwindow_sgl_load_proof.records[index] = MultiWindowSglLoadProofRecord{}
		}
		return generation
	}

	fn multiwindow_sgl_load_proof_snapshot_for_test(generation u64) !MultiWindowSglLoadProofSnapshot {
		if generation == 0 || !multiwindow_sgl_load_proof.armed
			|| multiwindow_sgl_load_proof.generation != generation {
			return error('gg.multiwindow: stale SGL load proof generation')
		}
		mut records := []MultiWindowSglLoadProofRecord{cap: multiwindow_sgl_load_proof.count}
		for index in 0 .. multiwindow_sgl_load_proof.count {
			records << multiwindow_sgl_load_proof.records[index]
		}
		return MultiWindowSglLoadProofSnapshot{
			generation: generation
			armed:      true
			overflow:   multiwindow_sgl_load_proof.overflow
			records:    records
		}
	}

	fn multiwindow_sgl_load_proof_disarm_for_test(generation u64) ! {
		if generation == 0 || !multiwindow_sgl_load_proof.armed
			|| multiwindow_sgl_load_proof.generation != generation {
			return error('gg.multiwindow: stale SGL load proof generation')
		}
		multiwindow_sgl_load_proof.armed = false
	}

	fn multiwindow_sgl_load_proof_record_for_test(pipeline render.Pipeline, window WindowId,
		context &render.Context, target_key string) {
		if !multiwindow_sgl_load_proof.armed || multiwindow_sgl_load_proof.overflow {
			return
		}
		if multiwindow_sgl_load_proof.count >= multiwindow_sgl_load_proof_capacity {
			multiwindow_sgl_load_proof.overflow = true
			return
		}
		index := multiwindow_sgl_load_proof.count
		multiwindow_sgl_load_proof.records[index] = MultiWindowSglLoadProofRecord{
			generation:  multiwindow_sgl_load_proof.generation
			sequence:    u64(index + 1)
			pipeline_id: pipeline.id
			window:      window
			// render.Context has no int id; its own address is a stable identity
			// for the lifetime of the context, and is never nil here.
			context_id:  unsafe { u64(uint(voidptr(context))) }
			target_key:  target_key
		}
		multiwindow_sgl_load_proof.count++
	}

	fn multiwindow_sgl_load_pipeline_for_test(pipeline render.Pipeline, window WindowId,
		context &render.Context, target_key string) {
		context.load_pipeline(pipeline)
		multiwindow_sgl_load_proof_record_for_test(pipeline, window, context, target_key)
	}
}
