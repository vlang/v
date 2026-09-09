module executor

// Error strings are centralized because they are part of the public contract.
// Keep them stable, short, and explicitly scoped to x.executor.
const err_drain_limit_invalid = 'executor: drain limit must be positive'
const err_executor_closed = 'executor: executor is closed'
const err_executor_stopped = 'executor: executor is stopped'
const err_nil_job = 'executor: job function is nil'
const err_not_owner_thread = 'executor: current thread is not the executor owner'
const err_owner_pump_running = 'executor: owner pump is already running'
const err_owner_submit_wait = 'executor: owner thread cannot wait for queue capacity'
const err_queue_full = 'executor: queue is full'
const err_queue_size_invalid = 'executor: queue size must be positive'
const err_timeout = 'executor: timeout'
const err_wait_before_terminal = 'executor: wait requires a running pump or stopped executor'
const err_wait_called = 'executor: wait was already called'
const err_wait_owner_thread = 'executor: wait cannot be called from executor owner thread'
