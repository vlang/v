module fastc

import os
import v.pref

fn test_fastc_memo_range_reads_fill_the_shared_buffer() {
	root := os.join_path(os.vtmp_dir(), 'fastc_memo_blob_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'resolve.memo')
	source := 'module main\nfn main() {}\n'
	header := 't'.repeat(fastc_memo_token_width)
	content := header + '\n' + source + '\n'
	os.write_file(fastc_memo_blob_path(path), content) or { panic(err) }
	stamp := FastcFileStamp{ size: i64(source.len), mtime: 10 }
	memo := FastcResolveMemo{
		files:      ['virtual.v']
		stamps:     [stamp]
		offsets:    [fastc_memo_token_width + 1]
		blob_token: header
		written:    20
	}
	prefs := pref.new_preferences()
	tasks, buffer := fastc_memo_blob_tasks(path, memo)
	assert tasks.len > 1
	mut results := []FastcMemoResult{}
	for index, task in tasks {
		assert task.blob.str == buffer.str
		results << fastc_run_memo_task(task, index, prefs, '')
	}
	assert fastc_memo_blob_from_results(tasks, results, memo, buffer) == content

	mut files := []string{}
	mut stamps := []FastcFileStamp{}
	mut offsets := []int{}
	for index in 0 .. fastc_source_load_chunk_size * 3 {
		files << 'virtual_${index}.v'
		stamps << stamp
		offsets << fastc_memo_token_width + 1
	}
	reads := fastc_memo_read_tasks(FastcResolveMemo{
		files:   files
		stamps:  stamps
		offsets: offsets
		written: 20
	}, stamps, buffer)
	assert reads.len > 1
	for index, task in reads {
		assert task.blob.str == buffer.str
		result := fastc_run_memo_task(task, index, prefs, '')
		assert result.sources.len == task.files.len
		for loaded in result.sources {
			assert !loaded.failed, loaded.error_message
			assert loaded.source == source
		}
	}
}
