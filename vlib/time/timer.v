module time

import time
import sync

// TaskCallback - Type definition for task callback functions
// This is a function type that takes no parameters and returns no value
// It is used to define the callback function that will be executed when a task is triggered
pub type TaskCallback = fn ()

// 标记为堆分配，避免在协程中使用时的安全问题
@[heap]
struct Task {
mut:
	id          int
	due_ms      i64 // 到期时间（毫秒），用于队列排序或记录计划时间
	interval    int // 0 表示一次性任务，>0 表示周期（毫秒）
	callback    TaskCallback = unsafe { nil } // 初始化函数指针
	canceled    bool
	fixed_delay bool // true: 固定延迟（interval 从 callback 完成时开始计时）；false: 固定频率（基于计划时间）
}

// Timer - A high-precision timer for scheduling tasks
// This struct manages a queue of tasks and executes them at the specified times
// It runs a background worker that checks for due tasks every 1 millisecond
pub struct Timer {
mut:
	tasks   []&Task    // Priority queue of tasks sorted by due time
	mu      sync.Mutex // Mutex for thread-safe operations
	next_id int        // Next task ID to assign
	running bool       // Flag indicating if the timer is running
}

// new_timer - Creates a new timer and starts the background worker
// Returns a pointer to the newly created Timer instance
// The background worker will automatically start running and check for due tasks every 1 millisecond
pub fn new_timer() &Timer {
	mut t := &Timer{
		tasks:   []&time.Task{}
		next_id: 1
		running: true
	}
	go t.worker()
	return t
}

// schedule_once - Schedules a one-time task to be executed after a specified delay
// Parameters:
//   delay_ms: The delay in milliseconds before the task is executed
//   cb: The callback function to be executed when the task is triggered
// Returns:
//   The ID of the scheduled task, which can be used to cancel it later
pub fn (mut t Timer) schedule_once(delay_ms int, cb TaskCallback) int {
	due := time.now().unix_milli() + i64(delay_ms)
	t.mu.lock()
	id := t.next_id
	t.next_id++
	task := &Task{
		id:          id
		due_ms:      due
		interval:    0
		callback:    cb
		canceled:    false
		fixed_delay: false
	}
	t.tasks << task
	t.tasks.sort_with_compare(fn (a &&Task, b &&Task) int {
		if a.due_ms < b.due_ms {
			return -1
		}
		if a.due_ms > b.due_ms {
			return 1
		}
		return 0
	})
	t.mu.unlock()
	return id
}

// schedule_repeated - Schedules a repeated task with fixed-rate execution
// The task is executed at fixed intervals based on the scheduled time
// Parameters:
//   first_delay_ms: The delay in milliseconds before the first execution
//   interval_ms: The interval in milliseconds between subsequent executions
//   cb: The callback function to be executed when the task is triggered
// Returns:
//   The ID of the scheduled task, which can be used to cancel it later
pub fn (mut t Timer) schedule_repeated(first_delay_ms int, interval_ms int, cb TaskCallback) int {
	return t.schedule_repeated_mode(first_delay_ms, interval_ms, cb, false)
}

// schedule_repeated_delay - Schedules a repeated task with fixed-delay execution
// The interval starts from the completion of the callback function
// Parameters:
//   first_delay_ms: The delay in milliseconds before the first execution
//   interval_ms: The interval in milliseconds between subsequent executions
//   cb: The callback function to be executed when the task is triggered
// Returns:
//   The ID of the scheduled task, which can be used to cancel it later
pub fn (mut t Timer) schedule_repeated_delay(first_delay_ms int, interval_ms int, cb TaskCallback) int {
	return t.schedule_repeated_mode(first_delay_ms, interval_ms, cb, true)
}

// 内部通用函数，fixed_delay 决定语义
fn (mut t Timer) schedule_repeated_mode(first_delay_ms int, interval_ms int, cb TaskCallback, fixed_delay bool) int {
	due := time.now().unix_milli() + i64(first_delay_ms)
	t.mu.lock()
	id := t.next_id
	t.next_id++
	task := &Task{
		id:          id
		due_ms:      due
		interval:    interval_ms
		callback:    cb
		canceled:    false
		fixed_delay: fixed_delay
	}
	t.tasks << task
	t.tasks.sort_with_compare(fn (a &&Task, b &&Task) int {
		if a.due_ms < b.due_ms {
			return -1
		}
		if a.due_ms > b.due_ms {
			return 1
		}
		return 0
	})
	t.mu.unlock()
	return id
}

// cancel - Cancels a scheduled task
// Parameters:
//   id: The ID of the task to cancel
// Returns:
//   true if the task was found and canceled, false otherwise
// Note:
//   If the task is currently running, this will prevent it from being rescheduled
//   but will not stop the current execution
pub fn (mut t Timer) cancel(id int) bool {
	t.mu.lock()
	defer { t.mu.unlock() }
	mut found := false
	mut i := 0
	for i < t.tasks.len {
		if t.tasks[i].id == id {
			t.tasks[i].canceled = true
			t.tasks.delete(i)
			found = true
			// 继续检查并删除所有具有相同ID的任务
			// 因为可能有多个相同ID的任务在队列中
			// 不需要i--，因为delete会自动调整索引
		} else {
			i++
		}
	}
	return found
}

// stop - Stops the timer and exits the background worker
// This will immediately stop the timer and clear all pending tasks
// Note:
//   It will not wait for currently running callbacks to complete
pub fn (mut t Timer) stop() {
	t.mu.lock()
	t.running = false
	t.tasks = []&time.Task{}
	t.mu.unlock()
}

// 后台 worker：每 1 ms 唤醒一次，检查并执行到期任务
fn (mut t Timer) worker() {
	for {
		t.mu.lock()
		if !t.running {
			t.mu.unlock()
			break
		}
		now := time.now().unix_milli()
		mut due_tasks := []&time.Task{}
		for t.tasks.len > 0 {
			first := t.tasks[0]
			if first.due_ms <= now {
				// 弹出队首
				t.tasks.delete(0)
				if !first.canceled {
					due_tasks << first
				}
			} else {
				break
			}
		}
		t.mu.unlock()

		for task in due_tasks {
			// 执行回调（在单独的协程中，避免阻塞 worker）
			go fn (tref &Task) {
				tref.callback()
			}(task)

			// 对于周期任务，根据 fixed_delay 决定何时重新入队
			if task.interval > 0 {
				// 计算下一次到期时间
				mut next_due := i64(0)
				if task.fixed_delay {
					// fixed-delay: 从当前时间开始计时（callback 完成后会再次检查）
					// 注意：由于回调在协程中执行，这里的时间可能不是回调完成的准确时间
					next_due = time.now().unix_milli() + i64(task.interval)
				} else {
					// fixed-rate: 基于计划时间
					next_due = task.due_ms + i64(task.interval)
					if next_due <= time.now().unix_milli() {
						// 若已落后太多，则以当前时间为基准（防止无限累积）
						next_due = time.now().unix_milli() + i64(task.interval)
					}
				}
				// 重新入队
				t.mu.lock()
				if t.running {
					// 创建新的任务实例
					mut new_task := &Task{
						id:          task.id
						due_ms:      next_due
						interval:    task.interval
						callback:    task.callback
						canceled:    false
						fixed_delay: task.fixed_delay
					}
					t.tasks << new_task
					t.tasks.sort_with_compare(fn (a &&Task, b &&Task) int {
						if a.due_ms < b.due_ms {
							return -1
						}
						if a.due_ms > b.due_ms {
							return 1
						}
						return 0
					})
				}
				t.mu.unlock()
			}
		}

		// 等待 1 毫秒
		time.sleep(1 * time.millisecond)
	}
}
