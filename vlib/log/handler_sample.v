// SampledHandler drops records according to a sampling policy.
module log

import context
import rand
import sync

// SampledHandler forwards only a fraction of records to its inner handler.
// Sampling is deterministic by default: one record in every `every` records is
// emitted. Set `random` to use a probabilistic decision instead.
pub struct SampledHandler {
	inner Handler
mut:
	mu      &sync.Mutex = sync.new_mutex()
	counter int
	every   int
	random  bool
}

// new_sampled_handler returns a handler that logs one record in every `every`.
pub fn new_sampled_handler(inner Handler, every int) &SampledHandler {
	if every < 1 {
		return &SampledHandler{ inner: inner, every: 1 }
	}
	return &SampledHandler{ inner: inner, every: every }
}

// new_random_sampled_handler returns a handler that logs each record with
// probability `1 / every`.
pub fn new_random_sampled_handler(inner Handler, every int) &SampledHandler {
	mut h := new_sampled_handler(inner, every)
	h.random = true
	return h
}

pub fn (h &SampledHandler) enabled(level Level) bool {
	return h.inner.enabled(level)
}

pub fn (h &SampledHandler) handle(ctx context.Context, mut r Record) ! {
	mut self := unsafe { h }
	self.mu.lock()
	defer { self.mu.unlock() }

	mut keep := false
	if self.random {
		keep = (rand.intn(self.every) or { 0 }) == 0
	} else {
		self.counter++
		keep = self.counter % self.every == 0
	}
	if keep {
		h.inner.handle(ctx, mut r) or {}
	}
}

pub fn (h &SampledHandler) with_attrs(attrs []Attr) Handler {
	return &SampledHandler{
		inner:  h.inner.with_attrs(attrs)
		every:  h.every
		random: h.random
	}
}

pub fn (h &SampledHandler) with_group(name string) Handler {
	return &SampledHandler{
		inner:  h.inner.with_group(name)
		every:  h.every
		random: h.random
	}
}

pub fn (h &SampledHandler) set_level(level Level) {
	h.inner.set_level(level)
}
