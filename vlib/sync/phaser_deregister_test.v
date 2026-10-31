module sync

import time

fn wait_for_phaser_arrival(mut p Phaser, expected u64) {
	deadline := time.now().add(5 * time.second)
	for {
		p.mutex.lock()
		arrived := p.arrived
		phase := p.phase
		p.mutex.unlock()
		assert phase == 0, 'phase advanced before every remaining party arrived'
		if arrived == expected {
			return
		}
		assert time.now() < deadline, 'arrival did not reach the barrier'
		time.sleep(time.millisecond)
	}
}

fn test_phaser_deregister_does_not_double_count_arrival() {
	mut p := new_phaser()
	for _ in 0 .. 3 {
		p.register()
	}
	assert p.arrive_and_deregister() == 0
	assert p.phase_number() == 0
	done := chan u64{cap: 1}
	spawn fn [mut p, done] () {
		done <- p.arrive_and_wait()
	}()
	wait_for_phaser_arrival(mut p, 1)
	assert p.arrive_and_wait() == 0
	assert <-done == 0
	assert p.phase_number() == 1
	// Both remaining parties leave the next phase; an empty deregistration is a no-op.
	assert p.arrive_and_deregister() == 1
	assert p.phase_number() == 1
	assert p.arrive_and_deregister() == 1
	assert p.phase_number() == 2
	assert p.arrive_and_deregister() == 2
	assert p.phase_number() == 2
}

fn test_phaser_deregister_releases_already_arrived_party() {
	mut p := new_phaser()
	for _ in 0 .. 3 {
		p.register()
	}
	done := chan u64{cap: 1}
	spawn fn [mut p, done] () {
		done <- p.arrive_and_wait()
	}()
	wait_for_phaser_arrival(mut p, 1)
	assert p.arrive_and_deregister() == 0
	assert p.phase_number() == 0
	assert p.arrive_and_deregister() == 0
	assert <-done == 0
	assert p.phase_number() == 1
}
