// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import net.http
import json
import sync.pool

struct Story {
	title string
	url   string
}

fn worker_fetch(p &pool.PoolProcessor, cursor int, worker_id int) voidptr {
	id := p.get_item<int>(cursor)
	resp := http.get('https://hacker-news.firebaseio.com/v0/item/${id}.json') or {
		println('failed to fetch data from /v0/item/${id}.json')
		return pool.no_result
	}
	story := json.decode(Story, resp.text) or {
		println('failed to decode a story')
		return pool.no_result
	}
	println('# $cursor) $story.title | $story.url')
	return pool.no_result
}

// Fetches top HN stories in parallel, depending on how many cores you have
fn main() {
	resp := http.get('https://hacker-news.firebaseio.com/v0/topstories.json') or {
		println('failed to fetch data from /v0/topstories.json')
		return
	}
	mut ids := json.decode([]int, resp.text) or {
		println('failed to decode topstories.json')
		return
	}
	if ids.len > 10 {
		ids = ids[0..10]
	}
	mut fetcher_pool := pool.new_pool_processor(
		callback: worker_fetch
	)
	// NB: if you do not call set_max_jobs, the pool will try to use an optimal
	// number of threads, one per each core in your system, which in most
	// cases is what you want anyway... You can override the automatic choice
	// by setting the VJOBS environment variable too.
	// fetcher_pool.set_max_jobs( 4 )
	fetcher_pool.work_on_items(ids)
}
