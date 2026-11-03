// These checks also run with -prod, which removes assert statements.
struct SharedGCContents {
mut:
	values map[string]string
}

struct SharedGCOwner {
mut:
	contents shared SharedGCContents
}

@[noinline]
fn allocate_shared_gc_batch(count int) {
	for i in 0 .. count {
		mut owner := SharedGCOwner{
			contents: SharedGCContents{
				values: {
					'payload': 'x'.repeat(4096)
				}
			}
		}
		lock owner.contents {
			if owner.contents.values['payload'].len != 4096 {
				panic('shared payload length changed')
			}
			owner.contents.values['sequence'] = i.str()
		}
	}
}

fn test_unreachable_shared_wrappers_release_their_payloads() {
	if !gc_is_enabled() { return }
	allocate_shared_gc_batch(16)
	gc_collect()
	before := gc_memory_use()
	allocate_shared_gc_batch(4096)
	gc_collect()
	after := gc_memory_use()
	if after > before + 1024 * 1024 {
		panic('Unreachable shared wrappers retained payloads: ${before} -> ${after}')
	}
}

fn shared_gc_waiter(shared contents SharedGCContents, ready chan bool, resume chan bool, done chan bool) {
	lock contents {
		ready <- true
		_ := <-resume
		if contents.values['payload'] != 'kept'.repeat(1024) {
			panic('worker lost shared payload during GC')
		}
		contents.values['worker'] = 'done'
	}
	done <- true
}

fn test_shared_alias_and_worker_survive_collection() {
	shared contents := SharedGCContents{
		values: {
			'payload': 'kept'.repeat(1024)
		}
	}
	owner := SharedGCOwner{ contents: contents }
	shared alias := owner.contents
	ready := chan bool{}
	resume := chan bool{}
	done := chan bool{}
	worker := spawn shared_gc_waiter(shared contents, ready, resume, done)
	is_ready := <-ready
	if !is_ready { panic('worker did not become ready') }
	allocate_shared_gc_batch(128)
	gc_collect()
	resume <- true
	is_done := <-done
	if !is_done { panic('worker did not complete') }
	worker.wait()
	rlock alias {
		if alias.values['worker'] != 'done' { panic('shared alias lost worker update') }
		if alias.values['payload'] != 'kept'.repeat(1024) {
			panic('shared alias lost payload during GC')
		}
	}
}

@[noinline]
fn start_shared_gc_waiter(ready chan bool, resume chan bool, done chan bool) thread {
	shared contents := SharedGCContents{
		values: {
			'payload': 'kept'.repeat(1024)
		}
	}
	return spawn shared_gc_waiter(shared contents, ready, resume, done)
}

fn test_worker_keeps_its_only_shared_reference_alive_during_collection() {
	ready := chan bool{}
	resume := chan bool{}
	done := chan bool{}
	worker := start_shared_gc_waiter(ready, resume, done)
	is_ready := <-ready
	if !is_ready { panic('worker did not become ready') }
	allocate_shared_gc_batch(128)
	gc_collect()
	resume <- true
	is_done := <-done
	if !is_done { panic('worker did not complete') }
	worker.wait()
}
