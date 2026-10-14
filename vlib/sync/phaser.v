module sync

// Phaser is a reusable barrier whose parties can join and leave at runtime.
// Every arrival (waiting or deregistering) counts toward the current phase;
// once all registered parties have arrived, the phase advances and every
// waiter is released. It is the V-idiomatic equivalent of Java's
// `java.util.concurrent.Phaser`.
@[heap]
pub struct Phaser {
mut:
	mutex   &Mutex
	cond    &Cond
	parties int
	arrived u64
	phase   u64
}

// new_phaser creates a Phaser with no registered parties.
pub fn new_phaser() &Phaser {
	mut m := new_mutex()
	return &Phaser{
		mutex: m
		cond:  new_cond(m)
	}
}

// register adds one party to the current phase.
pub fn (mut p Phaser) register() {
	p.mutex.lock()
	defer {
		p.mutex.unlock()
	}
	p.parties++
}

// arrive_and_wait records one arrival and blocks until every registered party
// has arrived, advancing the phase. It returns the completed phase number.
// With zero parties it returns the current phase immediately.
pub fn (mut p Phaser) arrive_and_wait() u64 {
	p.mutex.lock()
	defer {
		p.mutex.unlock()
	}
	if p.parties == 0 {
		return p.phase
	}
	ph := p.phase
	p.arrived++
	if p.arrived == u64(p.parties) {
		p.arrived = 0
		p.phase++
		p.cond.broadcast()
		return ph
	}
	for ph == p.phase {
		p.cond.wait()
	}
	return ph
}

// arrive_and_deregister records one arrival and removes one party. If that
// completes the phase, the phase advances and waiters are released. It
// returns the phase in which the party arrived, even if it is not yet complete.
pub fn (mut p Phaser) arrive_and_deregister() u64 {
	p.mutex.lock()
	defer {
		p.mutex.unlock()
	}
	ph := p.phase
	if p.parties == 0 {
		return ph
	}
	// Removing a party also removes its outstanding arrival for this phase.
	p.parties--
	if p.parties == 0 || p.arrived >= u64(p.parties) {
		p.arrived = 0
		p.phase++
		p.cond.broadcast()
	}
	return ph
}

// phase_number returns the current phase without arriving.
pub fn (p &Phaser) phase_number() u64 {
	p.mutex.lock()
	defer {
		p.mutex.unlock()
	}
	return p.phase
}
