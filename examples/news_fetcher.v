// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import net.http
import x.json2
import sync.pool

struct Story {
	title string
	url   string
}

fn worker_fetch(mut p pool.PoolProcessor, cursor int, worker_id int) voidptr {
	id := p.get_item[int](cursor)
	resp := http.get('https://hacker-news.firebaseio.com/v0/item/${id}.json') or {
		println('failed to fetch data from /v0/item/${id}.json')
		return pool.no_result
	}
	story := json2.decode[Story](resp.body) or {
		println('failed to decode a story')
		// println(resp.body)
		return pool.no_result
	}
	println('# ${cursor + 1}) ${story.title} | ${story.url}')
	return pool.no_result
}

// Fetches top HN stories in parallel, depending on how many cores you have
fn main() {
	resp := http.get('https://hacker-news.firebaseio.com/v0/topstories.json') or {
		println('failed to fetch data from /v0/topstories.json')
		return
	}
	// TODO bring back once json2 can decode []int
	/*
	ids := json2.decode[[]int](resp.body) or {
		println('failed to decode topstories.json $err')
		return
	}#[0..10]
	*/
	ids := resp.body.replace_once('[', '').replace_once(']', '').split(',').map(it.int())#[0..30]
	mut fetcher_pool := pool.new_pool_processor(
		callback: worker_fetch
	)
	// Note: if you do not call set_max_jobs, the pool will try to use an optimal
	// number of threads, one per each core in your system, which in most
	// cases is what you want anyway... You can override the automatic choice
	// by setting the VJOBS environment variable too.
	// fetcher_pool.set_max_jobs( 4 )
	fetcher_pool.work_on_items(ids)
}
