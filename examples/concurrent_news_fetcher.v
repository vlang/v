// Please share your thoughts, suggestions, questions, etc here:
// https://github.com/vlang-io/V/issues/3

// I'm very interested in your feedback.

module main

import http
import json
import runtime

struct Story {
        title string
}

// Fetches top HN stories in 8 coroutines
fn main() {
        resp := http.get('https://hacker-news.firebaseio.com/v0/topstories.json') or {
                panic(err)
        }
        ids := json.decode([]int, resp.body) or {
                panic(err)
        }
        mut cursor := 0
        for i := 0; i < 8; i++ {
                go fn() {
                        for cursor < ids.len {
                                lock { // Without this lock block the program will not compile
                                        id := ids[cursor]
                                        cursor++
                                }
                                url := 'https://hacker-news.firebaseio.com/v0/item/$id.json'
                                resp := http.get(url) or {
                                        panic(err)
                                }
                                story := json.decode(Story, resp.body) or {
                                        panic(err)
                                }
                                println(story.title)
                        }
                }()
        }
        runtime.wait() // Waits for all coroutines to finish
}
