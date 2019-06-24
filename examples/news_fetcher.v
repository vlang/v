// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import http
import json
import sync
import time

const (
	NR_THREADS = 8
)

struct Story {
	title string
	url string
}

struct Fetcher {
mut:
	mu     sync.Mutex
	ids    []int
	cursor int
}

fn (f mut Fetcher) fetch() {
	for {
		f.mu.lock()
		if f.cursor >= f.ids.len {
			return
		}
		id := f.ids[f.cursor]
		f.cursor++
		f.mu.unlock()
		resp := http.get('https://hacker-news.firebaseio.com/v0/item/${id}.json')
		story := json.decode(Story, resp) or {
			println('failed to decode a story')
			exit(1)
		}
		println('#$f.cursor) $story.title | $story.url')
	}
}

// Fetches top HN stories in 8 coroutines
fn main() {
	resp := http.get('https://hacker-news.firebaseio.com/v0/topstories.json')
	ids := json.decode( []int, resp) or {
		println('failed to fetch topstories.json')
		return
	}
	fetcher := &Fetcher{ids: ids}
	for i := 0; i < NR_THREADS; i++ {
		go fetcher.fetch()
	}
	println(fetcher.ids)
	time.sleep(5)
}

