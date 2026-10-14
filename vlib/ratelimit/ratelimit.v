module ratelimit

import math
import sync
import time

// TokenBucketConfig configures a TokenBucket.
pub struct TokenBucketConfig {
pub:
	// sustained rate in tokens (requests) per second
	rate_per_sec f64
	// maximum burst: bucket capacity and initial tokens
	burst int
}

// BucketStats counts allowed and denied requests.
pub struct BucketStats {
pub:
	allowed u64
	denied  u64
}

// TokenBucket is a thread-safe token-bucket rate limiter: tokens refill at
// rate_per_sec up to burst, and each allowed request costs one token. It is
// the V-idiomatic equivalent of .NET's token-bucket limiter, for veb/fasthttp
// handlers and metered API clients.
@[heap]
pub struct TokenBucket {
mut:
	mutex   &sync.Mutex
	rate    f64
	cap     f64
	tokens  f64
	last    i64 // millisecond timestamp of the last refill
	now     fn () i64
	allowed u64
	denied  u64
}

// new_token_bucket creates a full bucket reading a monotonic millisecond clock.
pub fn new_token_bucket(config TokenBucketConfig) &TokenBucket {
	return new_token_bucket_with_clock(config, fn () i64 {
		return i64(time.sys_mono_now() / u64(time.millisecond))
	})
}

// new_token_bucket_with_clock is new_token_bucket with an injectable
// millisecond clock, for deterministic tests and custom time sources.
pub fn new_token_bucket_with_clock(config TokenBucketConfig, now fn () i64) &TokenBucket {
	assert config.rate_per_sec > 0 && !math.is_inf(config.rate_per_sec, 0), 'ratelimit: rate_per_sec must be positive and finite'
	assert config.burst > 0, 'ratelimit: burst must be positive'
	return &TokenBucket{
		mutex:  sync.new_mutex()
		rate:   config.rate_per_sec
		cap:    f64(config.burst)
		tokens: f64(config.burst)
		last:   now()
		now:    now
	}
}

// allow consumes one token if available, refilling first from the elapsed
// time. It returns false (counted in stats) when empty.
pub fn (mut b TokenBucket) allow() bool {
	b.mutex.lock()
	defer {
		b.mutex.unlock()
	}
	now := b.now()
	// Ignore backwards clock movement without granting the same interval twice.
	if now > b.last {
		elapsed := f64(now - b.last) / 1000.0
		b.last = now
		b.tokens += elapsed * b.rate
		if b.tokens > b.cap {
			b.tokens = b.cap
		}
	}
	// The epsilon absorbs float dust at exact refill boundaries (e.g. 100ms
	// at 10/s), regardless of backend FP flags; it is far below one token.
	if b.tokens + 1e-9 >= 1.0 {
		b.tokens -= 1.0
		if b.tokens < 0 {
			b.tokens = 0
		}
		b.allowed++
		return true
	}
	b.denied++
	return false
}

// stats returns the allowed/denied counters.
pub fn (b &TokenBucket) stats() BucketStats {
	b.mutex.lock()
	defer {
		b.mutex.unlock()
	}
	return BucketStats{b.allowed, b.denied}
}
