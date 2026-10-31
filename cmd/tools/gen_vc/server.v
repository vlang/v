module main

import net.http
import json2
import crypto.hmac
import crypto.sha256
import time

struct PushRepository {
	full_name string
}

// PushEvent is the part of a GitHub `push` webhook payload that gen_vc reads. The payload only
// triggers a run; what is built is always the master that the run fetches itself.
struct PushEvent {
	ref        string
	repository PushRepository
}

struct ServiceState {
mut:
	running bool
	reason  string
	since   time.Time
	runs    int
	last    Report
	log     []string
}

struct StatusJson {
	mode           string
	running        bool
	reason         string
	since          string
	runs           int
	last_outcome   string
	last_v_commit  string
	last_vc_commit string
	last_message   string
	last_started   string
	last_finished  string
}

@[heap]
struct Service {
	config Config
	// trigger holds at most one pending run. Requests made while a run is pending coalesce into it,
	// like a concurrency group that skips the runs queued between the running and the latest one.
	trigger chan string = chan string{cap: 1}
mut:
	state shared ServiceState
}

// request_run queues a run unless one is already pending, and reports whether it queued one.
fn (mut s Service) request_run(reason string) bool {
	select {
		s.trigger <- reason {
			return true
		}
		else {
			return false
		}
	}
	return false
}

fn (mut s Service) work() {
	mut runner := Runner{
		config: s.config
	}
	for {
		reason := <-s.trigger or { return }
		lock s.state {
			s.state.running = true
			s.state.reason = reason
			s.state.since = time.now()
		}
		report := runner.run(false)
		lock s.state {
			s.state.running = false
			s.state.runs++
			s.state.last = report
			s.state.log = runner.log.lines.clone()
		}
	}
}

fn (mut s Service) poll() {
	for {
		time.sleep(s.config.poll_minutes * time.minute)
		s.request_run('poll')
	}
}

fn format_time(t time.Time) string {
	return if t.unix() == 0 { '' } else { t.format_rfc3339() }
}

fn (mut s Service) status() StatusJson {
	rlock s.state {
		return StatusJson{
			mode:           s.config.mode.str()
			running:        s.state.running
			reason:         s.state.reason
			since:          format_time(s.state.since)
			runs:           s.state.runs
			last_outcome:   if s.state.runs == 0 { '' } else { s.state.last.outcome.str() }
			last_v_commit:  s.state.last.v_commit
			last_vc_commit: s.state.last.vc_commit
			last_message:   s.state.last.message
			last_started:   format_time(s.state.last.started)
			last_finished:  format_time(s.state.last.finished)
		}
	}
	return StatusJson{}
}

fn (mut s Service) status_text() string {
	st := s.status()
	mut lines := ['vcgen.vlang.io: regenerates vlang/vc from vlang/v master', 'mode: ${st.mode}']
	if st.running {
		lines << 'state: running (${st.reason}, since ${st.since})'
	} else {
		lines << 'state: idle'
	}
	lines << 'runs: ${st.runs}'
	if st.runs > 0 {
		lines << 'last run: ${st.last_outcome} (${st.last_started} .. ${st.last_finished})'
		lines << 'vlang/v: ${st.last_v_commit}'
		lines << 'vlang/vc: ${st.last_vc_commit}'
		lines << st.last_message
	}
	lines << 'log of the last run: /log, status as JSON: /status.json'
	return lines.join('\n') + '\n'
}

fn (mut s Service) last_log() string {
	rlock s.state {
		return s.state.log.join('\n') + '\n'
	}
	return ''
}

// valid_signature checks GitHub's `X-Hub-Signature-256: sha256=<hex>` header, an HMAC-SHA256 of
// the exact request body keyed with the webhook secret, in constant time.
fn valid_signature(secret string, body string, header string) bool {
	if secret == '' || !header.starts_with('sha256=') {
		return false
	}
	expected := hmac.new(secret.bytes(), body.bytes(), sha256.sum, sha256.block_size).hex()
	return hmac.equal(expected.bytes(), header['sha256='.len..].bytes())
}

fn text_response(status http.Status, body string) http.Response {
	return http.new_response(
		status: status
		body:   body
		header: http.new_header(key: .content_type, value: 'text/plain; charset=utf-8')
	)
}

fn (mut s Service) webhook(req http.Request) http.Response {
	if req.method != .post {
		return text_response(.method_not_allowed, 'POST only\n')
	}
	if s.config.webhook_secret == '' {
		return text_response(.service_unavailable, 'the webhook is disabled: VCGEN_WEBHOOK_SECRET is not set\n')
	}
	signature := req.header.get_custom('X-Hub-Signature-256') or { '' }
	if !valid_signature(s.config.webhook_secret, req.data, signature) {
		return text_response(.unauthorized, 'invalid signature\n')
	}
	event := req.header.get_custom('X-GitHub-Event') or { '' }
	if event == 'ping' {
		return text_response(.ok, 'pong\n')
	}
	if event != 'push' {
		return text_response(.ok, 'ignored: not a push event\n')
	}
	payload := json2.decode[PushEvent](req.data) or {
		return text_response(.bad_request, 'invalid push payload\n')
	}
	if s.config.repository == '' {
		return text_response(.ok, 'ignored: --v-url is not a github.com repository\n')
	}
	if payload.ref != 'refs/heads/master' || payload.repository.full_name != s.config.repository {
		return text_response(.ok, 'ignored: not a push to ${s.config.repository} master\n')
	}
	if s.request_run('push') {
		return text_response(.accepted, 'queued\n')
	}
	return text_response(.accepted, 'a run is already queued\n')
}

struct WebHandler {
mut:
	service &Service = unsafe { nil }
}

fn (mut h WebHandler) handle(req http.Request) http.Response {
	path := req.url.all_before('?')
	if path == '/genhook' {
		return h.service.webhook(req)
	}
	if path !in ['/', '/status.json', '/log'] {
		return text_response(.not_found, 'not found\n')
	}
	if req.method !in [.get, .head] {
		return text_response(.method_not_allowed, 'GET only\n')
	}
	return match path {
		'/status.json' {
			http.new_response(
				body:   json2.encode(h.service.status()) + '\n'
				header: http.new_header(key: .content_type, value: 'application/json')
			)
		}
		'/log' {
			text_response(.ok, h.service.last_log())
		}
		else {
			text_response(.ok, h.service.status_text())
		}
	}
}

// serve runs the service: one worker thread performs the runs, a poller queues one every
// `--poll-minutes`, and the HTTP server shows the status and accepts GitHub's push webhook.
fn serve(config Config) ! {
	mut service := &Service{
		config: config
	}
	spawn service.work()
	if config.poll_minutes > 0 {
		spawn service.poll()
	}
	service.request_run('start')
	mut server := &http.Server{
		addr:    '${config.host}:${config.port}'
		handler: WebHandler{
			service: service
		}
	}
	server.listen_and_serve()
}
