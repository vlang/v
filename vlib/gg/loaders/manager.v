module loaders

// LoadingManager tracks a batch of asset loads, reports progress and keeps a
// small text cache. It mirrors three.js's LoadingManager without any IO policy:
// your loader calls begin/progress/finish around its work.
pub struct LoadingManager {
pub mut:
	items_loaded int
	items_total  int
	cache        map[string]string
	on_start     fn () = manager_noop
	on_progress  fn (loaded int, total int, path string) = manager_progress_noop
	on_load      fn ()                        = manager_noop
	on_error     fn (path string, msg string) = manager_error_noop
}

fn manager_noop() {}

fn manager_progress_noop(loaded int, total int, path string) {
	_ = loaded
	_ = total
	_ = path
}

fn manager_error_noop(path string, msg string) {
	_ = path
	_ = msg
}

// new_loading_manager returns an empty manager with no-op callbacks.
pub fn new_loading_manager() LoadingManager {
	return LoadingManager{}
}

// begin starts a batch of total items.
pub fn (mut m LoadingManager) begin(total int) {
	m.items_total = if total < 0 { 0 } else { total }
	m.items_loaded = 0
	m.on_start()
}

// progress records one completed item and reports the updated counts.
pub fn (mut m LoadingManager) progress(path string) {
	m.items_loaded++
	m.on_progress(m.items_loaded, m.items_total, path)
}

// finish reports that the batch is complete.
pub fn (m &LoadingManager) finish() {
	m.on_load()
}

// fail reports an error for one path.
pub fn (m &LoadingManager) fail(path string, msg string) {
	m.on_error(path, msg)
}

// put stores text in the cache.
pub fn (mut m LoadingManager) put(path string, data string) {
	m.cache[path] = data
}

// get returns cached text or an empty string.
pub fn (m &LoadingManager) get(path string) string {
	return m.cache[path] or { '' }
}

// has reports whether a path is cached.
pub fn (m &LoadingManager) has(path string) bool {
	return path in m.cache
}

// clear_cache removes all cached entries.
pub fn (mut m LoadingManager) clear_cache() {
	m.cache = map[string]string{}
}

// fraction returns batch progress in [0, 1].
pub fn (m &LoadingManager) fraction() f32 {
	if m.items_total <= 0 {
		return 0
	}
	v := f32(m.items_loaded) / f32(m.items_total)
	return if v > 1 { 1 } else { v }
}
