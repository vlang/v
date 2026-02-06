// This example illustrates how to use `sync.pool`,
// and how the various settings for VJOBS, work items etc can interact.
@[has_globals]
module main

import log
import time
import runtime
import sync.pool

const args = arguments()
const nitems = args[1] or { '10' }.int()
const njobs = args[2] or { runtime.nr_jobs().str() }.int()
const delay = args[3] or { '1000' }.int()

__global msgs = chan string{cap: 1000}

fn worker_sleep(mut p pool.PoolProcessor, item_idx int, worker_id int) voidptr {
	item := p.get_item[int](item_idx)
	msgs <- '# worker_id: ${worker_id:3}, item_idx: ${item_idx + 1:03}, item: ${item:6}, started'
	time.sleep(delay * time.millisecond)
	msgs <- '# worker_id: ${worker_id:3}, item_idx: ${item_idx + 1:03}, item: ${item:6}, finished.'
	return pool.no_result
}

fn logger() {
	for {
		msg := <-msgs
		log.info(msg)
		if msg == '>>> done' {
			break
		}
	}
}

fn main() {
	t := spawn logger()
	msgs <- '>>> nitems: ${nitems:6} | njobs: ${njobs:6} | delay_ms: ${delay:6}'
	items := []int{len: nitems, init: index * 1000}
	mut fetcher_pool := pool.new_pool_processor(callback: worker_sleep)
	fetcher_pool.work_on_items(items)
	msgs <- '>>> done'
	t.wait()
}
