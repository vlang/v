import net.http
import json
import os

// --- Data Models ---

struct Part {
pub:
	text string @[json: 'text']
}

struct Content {
pub:
	role  string @[json: 'role']
	parts []Part @[json: 'parts']
}

struct GenerateRequest {
pub:
	contents []Content @[json: 'contents']
}

struct GenerateResponse {
pub:
	candidates []Candidate @[json: 'candidates']
}

struct Candidate {
pub:
	content Content @[json: 'content']
}

// Execution Context
struct AppContext {
mut:
	buffer    string  // SSE parsing buffer
	full_resp string  // Accumulated AI response
	out       os.File // Standard output
}

// Core Streaming Callback - Enhanced Parsing
fn stream_callback(request &http.Request, chunk []u8, read_so_far u64, expected_size u64, status_code int) ! {
	mut ctx := unsafe { &AppContext(request.user_ptr) }
	ctx.buffer += chunk.bytestr()

	for {
		// Look for the start of the data block
		start_idx := ctx.buffer.index('data: ') or { break }

		// Look for the end of the line (\n)
		end_idx := ctx.buffer.index_after('\n', start_idx) or { -1 }
		if end_idx == -1 {
			break
		} // Incomplete data, wait for next chunk

		raw_line := ctx.buffer[start_idx..end_idx].trim_space()

		// Remove the processed part from the buffer
		ctx.buffer = ctx.buffer[end_idx + 1..]

		if raw_line.starts_with('data: ') {
			json_str := raw_line[6..].trim_space()
			if json_str == '' || json_str == '[DONE]' {
				continue
			}

			resp := json.decode(GenerateResponse, json_str) or { continue }

			if resp.candidates.len > 0 && resp.candidates[0].content.parts.len > 0 {
				txt := resp.candidates[0].content.parts[0].text
				if txt != '' {
					print(txt)
					ctx.out.flush()
					ctx.full_resp += txt
				}
			}
		}
	}
}

fn main() {
	api_key := os.getenv('GEMINI_API_KEY')
	if api_key == '' {
		eprintln('Error: GEMINI_API_KEY environment variable not set.')
		exit(1)
	}

	// Automatically get proxy settings from environment variables
	mut proxy_url := os.getenv('HTTPS_PROXY')
	if proxy_url == '' {
		proxy_url = os.getenv('http_proxy')
	}

	model := 'gemini-3-flash-preview'
	api_url := 'https://generativelanguage.googleapis.com/v1beta/models/${model}:streamGenerateContent?alt=sse&key=${api_key}'

	mut history := []Content{}
	mut ctx := &AppContext{
		out: os.stdout()
	}

	println('--- Gemini Native V-CLI ---')
	if proxy_url != '' {
		println('Using Proxy: $proxy_url')
	}
	println('Model: $model')
	println('Type "exit" to quit.\n')

	for {
		print('You > ')
		ctx.out.flush()
		input := os.get_line().trim_space()

		if input in ['exit', 'quit'] {
			break
		}
		if input == '' {
			continue
		}

		history << Content{
			role: 'user'
			parts: [Part{text: input}]
		}

		print('Gemini > ')
		ctx.out.flush()

		// Reset context state for the new turn
		ctx.full_resp = ''
		ctx.buffer = ''

		mut req := http.Request{
			url:                api_url
			method:             .post
			data:               json.encode(GenerateRequest{
				contents: history
			})
			user_ptr:           ctx
			on_progress_body:   stream_callback
			stop_copying_limit: 0
			validate:           false
		}

		if proxy_url != '' {
			req.proxy = http.new_http_proxy(proxy_url) or { unsafe { nil } }
		}

		req.add_header(.content_type, 'application/json')
		req.add_header(.accept, 'text/event-stream')

		req.do() or {
			eprintln('\nRequest Error: $err')
			continue
		}

		// Save AI response to history
	history << Content{
			role:  'model'
			parts: [Part{text: ctx.full_resp}]
		}
		println('
')
	}
}
