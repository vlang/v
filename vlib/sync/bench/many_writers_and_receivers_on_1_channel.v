import os
import os.cmdline
import time
import sync

// Usage:
// many_writers_and_receivers_on_1_channel [-readers 1] [-writers 4] [-chan_cap 100] [-iterations 25000] > results.csv
//
// You can then open results.csv in Excel/Calc and for example plot the first vs the second column.
enum EventKind {
	push
	pop
}

struct Event {
	is_set  bool
	id      int
	gtime   u64 // nanoseconds
	i       int
	kind    EventKind
	elapsed i64 // nanoseconds, elapsed after the previous event of the same kind
}

struct Context {
mut:
	n_iters   int
	n_readers int
	n_writers int
	//
	pops_wg &sync.WaitGroup
	pops    []Event
	//
	pushes_wg &sync.WaitGroup
	pushes    []Event
}

fn do_rec(ch chan int, id int, mut ctx Context) {
	eprintln('start of  do_rec id: ${id}')
	mut timer_sw_x := time.new_stopwatch()
	mut tmp := int(0)
	mut i := int(0)
	// Note: a single receiver thread can get slightly more
	// than its fair share of sends, that is why
	// the receiver's Event array is much larger,
	// enough so a single receiver can potentially process all
	// writers pushes, and it is partitioned over all of
	// id, ctx.n_writers and n_iters:
	n_iters := ctx.n_iters
	base := id * n_iters * ctx.n_writers
	for {
		for ch.try_pop(tmp) == .success {
			ctx.pops[base + i] = Event{
				is_set: true
				id: id
				gtime: time.sys_mono_now()
				i: i
				kind: .pop
				elapsed: timer_sw_x.elapsed().nanoseconds()
			}
			timer_sw_x.restart()
			i++
			if tmp == 1 {
				ctx.pops_wg.done()
				return
			}
		}
	}
}

fn do_send(ch chan int, id int, mut ctx Context) {
	eprintln('start of do_send id: ${id}')
	mut timer_sw_x := time.new_stopwatch()
	n_iters := ctx.n_iters
	base := n_iters * id // sender events can not overlap
	for i := 0; i < n_iters; i++ {
		idx := base + i
		ctx.pushes[idx] = Event{
			is_set: true
			id: id
			gtime: time.sys_mono_now()
			i: i
			kind: .push
			elapsed: timer_sw_x.elapsed().nanoseconds()
		}
		timer_sw_x.restart()
		tmp := int(0)
		ch <- tmp
	}
	ctx.pushes_wg.done()
}

fn main() {
	//
	args := os.args[1..]
	if '-h' in args || '--help' in args {
		eprintln('Usage:\n many_writers_and_receivers_on_1_channel [-readers 1] [-writers 4] [-chan_cap 100] [-iterations 25000]')
		exit(0)
	}
	n_iters := cmdline.option(args, '-iterations', '25000').int()
	n_readers := cmdline.option(args, '-readers', '1').int()
	n_writers := cmdline.option(args, '-writers', '4').int()
	chan_cap := cmdline.option(args, '-chan_cap', '100').int()
	eprintln('> n_iters, ${n_iters}, n_writers, ${n_writers}, n_readers, ${n_readers}, chan_cap, ${chan_cap}')
	//
	ch := chan int{cap: chan_cap}
	max_number_of_pushes := n_writers * (n_iters + 2)
	max_number_of_pops := max_number_of_pushes * n_readers
	eprintln('> max_number_of_pushes, ${max_number_of_pushes}, max_number_of_pops (per receiver), ${max_number_of_pops}')
	mut ctx := &Context{
		n_iters: n_iters
		n_readers: n_readers
		n_writers: n_writers
		pushes_wg: sync.new_waitgroup()
		pops_wg: sync.new_waitgroup()
		pushes: []Event{len: max_number_of_pushes}
		pops: []Event{len: max_number_of_pops}
	}
	ctx.pops_wg.add(n_readers)
	for i := 0; i < n_readers; i++ {
		spawn do_rec(ch, i, mut ctx)
	}
	ctx.pushes_wg.add(n_writers)
	for i := 0; i < n_writers; i++ {
		spawn do_send(ch, i, mut ctx)
	}
	ctx.pushes_wg.wait()
	eprintln('>> all pushes done')
	for i := 0; i < n_readers; i++ {
		ch <- 1
	}
	ctx.pops_wg.wait()
	eprintln('>> all pops done')
	mut all_events := []Event{}
	all_events << ctx.pops
	all_events << ctx.pushes
	all_events.sort(a.elapsed < b.elapsed)
	mut i := 0
	for e in all_events {
		if !e.is_set {
			continue
		}
		i++
		if e.kind == .pop {
			println('${i:8} , ${e.elapsed:10}, ns ,  do_rec id:, ${e.id:3} , i=, ${e.i:5} , ${e.gtime:20}')
		}
		if e.kind == .push {
			println('${i:8} , ${e.elapsed:10}, ns , do_send id:, ${e.id:3} , i=, ${e.i:5} , ${e.gtime:20}')
		}
	}
}
