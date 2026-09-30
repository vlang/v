type IntWorker = thread int

type NestedIntWorker = IntWorker

type VoidWorker = thread

fn thread_alias_compute(n int) int {
	return n * 2
}

fn thread_alias_tick() {}

fn test_builtin_wait_collects_results_from_thread_handle_aliases() {
	mut workers := []NestedIntWorker{}
	workers << NestedIntWorker(spawn thread_alias_compute(1))
	workers << NestedIntWorker(spawn thread_alias_compute(2))
	assert workers.wait() == [2, 4]
}

fn test_builtin_wait_joins_void_thread_handle_aliases() {
	mut workers := []VoidWorker{}
	workers << VoidWorker(spawn thread_alias_tick())
	workers.wait()
}
