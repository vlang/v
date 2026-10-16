module veb

import sync
import time

// RateLimitOptions configures the `rate_limit` middleware: a token bucket per
// client key, refilled at `requests_per_second` up to `burst` tokens. The key
// is the request IP. Once the table reaches `max_keys`, buckets idle longer
// than `idle_timeout` are swept first, so distinct clients cannot grow it
// without bound.
@[params]
pub struct RateLimitOptions {
pub:
	requests_per_second f64           = 10
	burst               int           = 20
	max_keys            int           = 10000
	idle_timeout        time.Duration = time.hour
	message             string        = '429 Too Many Requests'
}

struct TokenBucket {
mut:
	tokens       f64
	last_unix_ms i64
}

// RateLimitStore holds one bucket per key. veb serves requests on worker
// threads, so every access goes through the mutex; the bucket math itself is
// pure and tested without threads.
struct RateLimitStore {
mut:
	mu      &sync.Mutex
	buckets map[string]TokenBucket
}

fn new_rate_limit_store() &RateLimitStore {
	return &RateLimitStore{
		mu: sync.new_mutex()
	}
}

// allow consumes one token for `key` when the bucket holds one after
// refilling to `now_ms`. It returns whether the request passes and, when it
// does not, after how many whole seconds the next token arrives, which is
// what the `Retry-After` header carries.
fn (mut s RateLimitStore) allow(key string, now_ms i64, o RateLimitOptions) (bool, int) {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	if s.buckets.len >= o.max_keys {
		s.sweep_idle(now_ms, o.idle_timeout)
	}
	mut bucket := s.buckets[key] or {
		TokenBucket{
			tokens:       f64(o.burst)
			last_unix_ms: now_ms
		}
	}
	allowed, retry_after := bucket_allow(mut bucket, now_ms, o.requests_per_second,
		o.burst)
	s.buckets[key] = bucket
	return allowed, retry_after
}

// sweep_idle drops buckets untouched for longer than `idle`, bounding the
// table a flood of distinct keys can build.
fn (mut s RateLimitStore) sweep_idle(now_ms i64, idle time.Duration) {
	idle_ms := i64(idle) / i64(time.millisecond)
	for key, bucket in s.buckets {
		if now_ms - bucket.last_unix_ms > idle_ms {
			s.buckets.delete(key)
		}
	}
}

// bucket_allow refills `b` to `now_ms` and consumes one token when one is
// available. A non-positive rate never refills; the request is still denied
// rather than dividing by zero.
fn bucket_allow(mut b TokenBucket, now_ms i64, rate_per_sec f64, burst int) (bool, int) {
	elapsed := now_ms - b.last_unix_ms
	if elapsed > 0 && rate_per_sec > 0 {
		b.tokens += f64(elapsed) / 1000.0 * rate_per_sec
		if b.tokens > f64(burst) {
			b.tokens = f64(burst)
		}
		b.last_unix_ms = now_ms
	}
	if b.tokens >= 1.0 {
		b.tokens -= 1.0
		return true, 0
	}
	if rate_per_sec <= 0 {
		return false, 60
	}
	retry_after := int((1.0 - b.tokens) / rate_per_sec) + 1
	return false, retry_after
}

// rate_limit returns middleware that caps requests per client key at the
// configured rate, answering 429 with a `Retry-After` header past it.
// Register it globally or on the routes that need it:
// ```v
// app.use(veb.rate_limit[Context](requests_per_second: 5, burst: 10))
// ```
pub fn rate_limit[T](options RateLimitOptions) MiddlewareOptions[T] {
	store := new_rate_limit_store()
	return MiddlewareOptions[T]{
		handler: fn [options, store] [T](mut ctx T) bool {
			allowed, retry_after := store.allow(ctx.Context.ip(), time.now().unix_milli(),
				options)
			if allowed {
				return true
			}
			ctx.res.set_status(.too_many_requests)
			ctx.res.header.set_custom('Retry-After', retry_after.str()) or {}
			ctx.text(options.message)
			return false
		}
	}
}
