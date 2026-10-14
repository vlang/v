import sync
import sync.stdatomic { new_atomic }

// Three parties rendezvous over two phases; the returned phase numbers track.
fn test_phaser_two_phases() {
	mut p := sync.new_phaser()
	p.register()
	p.register()
	p.register()
	mut arrived := new_atomic(0)
	done_ch := chan bool{cap: 2}
	defer {
		done_ch.close()
	}
	for _ in 0 .. 2 {
		spawn fn [mut p, mut arrived, done_ch] () {
			assert p.arrive_and_wait() == 0, 'first rendezvous completes phase 0'
			arrived.add(1)
			assert p.arrive_and_wait() == 1, 'second rendezvous completes phase 1'
			arrived.add(1)
			done_ch <- true
		}()
	}
	assert p.arrive_and_wait() == 0
	arrived.add(1)
	assert p.arrive_and_wait() == 1
	arrived.add(1)
	for _ in 0 .. 2 {
		_ := <-done_ch
	}
	assert arrived.load() == 6, 'all three parties must pass both phases'
	assert p.phase_number() == 2
}

// A deregistering arrival can complete the phase for the waiter left behind.
fn test_phaser_deregister_shrinks_parties() {
	mut p := sync.new_phaser()
	p.register()
	p.register()
	done_ch := chan bool{cap: 1}
	defer {
		done_ch.close()
	}
	spawn fn [mut p, done_ch] () {
		assert p.arrive_and_deregister() == 0
		done_ch <- true
	}()
	assert p.arrive_and_wait() == 0, 'deregistering arrival must release the waiter'
	_ := <-done_ch
	assert p.phase_number() == 1
	// one party left: arrives pass straight through
	assert p.arrive_and_wait() == 1
	assert p.phase_number() == 2
}

// Zero parties: arrival is a no-op returning the current phase.
fn test_phaser_empty_passes_through() {
	mut p := sync.new_phaser()
	assert p.arrive_and_wait() == 0
	assert p.phase_number() == 0
}
