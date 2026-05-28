module async

// Error strings are centralized because they form the observable contract for
// callers and tests. Keep them short, stable, and explicit about whether the
// failure came from x.async or from the parent context.
const err_group_go_after_wait = 'async: group does not accept new tasks after wait starts'
const err_group_wait_called = 'async: group wait was already called'
const err_interval_invalid = 'async: interval must be positive'
const err_nil_job = 'async: job function is nil'
const err_pool_closed = 'async: pool is closed'
const err_pool_queue_full = 'async: pool queue is full'
const err_pool_queue_size_invalid = 'async: pool queue size must be positive'
const err_pool_wait_called = 'async: pool wait was already called'
const err_pool_workers_invalid = 'async: pool worker count must be positive'
const err_task_wait_called = 'async: task wait was already called'
const err_timeout = 'async: timeout'

const context_canceled = 'context canceled'

// The current context module exposes deadline expiry as an IError with this
// message. x.async normalizes that specific deadline error to `async: timeout`.
const context_deadline_exceeded = 'context deadline exceeded'
