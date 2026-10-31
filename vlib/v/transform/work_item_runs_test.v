module transform

fn file_run_test_items() []FnWorkItem {
	mut items := []FnWorkItem{}
	for i in 0 .. 400 {
		items << FnWorkItem{
			fn_idx: i
			file:   'f${i / 20}.v'
			cost:   if i % 37 == 0 { 300 } else { 10 + i % 7 }
		}
	}
	return items
}

fn test_file_runs_assign_every_item_once_in_source_order() {
	items := file_run_test_items()
	for jobs in [2, 4, 8] {
		for share in [100, 85] {
			buckets := split_work_items_by_file_runs(items, jobs, share)
			assert buckets.len == jobs
			mut seen := []bool{len: items.len}
			for bucket in buckets {
				assert bucket.len > 0
				for i, item in bucket {
					assert !seen[item.fn_idx]
					seen[item.fn_idx] = true
					if i > 0 {
						assert item.fn_idx > bucket[i - 1].fn_idx
					}
				}
			}
			for flag in seen {
				assert flag
			}
		}
	}
}

fn test_file_runs_keep_files_together_when_runs_fit() {
	items := file_run_test_items()
	buckets := split_work_items_by_file_runs(items, 2, 100)
	mut owner := map[string]int{}
	mut split_files := 0
	for b, bucket in buckets {
		for item in bucket {
			if prev := owner[item.file] {
				if prev != b {
					split_files++
				}
			} else {
				owner[item.file] = b
			}
		}
	}
	// Only files whose run exceeds the cost cap may span buckets.
	assert split_files < 20
}

fn test_file_runs_fall_back_for_few_items() {
	items := file_run_test_items()[..5]
	buckets := split_work_items_by_file_runs(items, 4, 100)
	mut count := 0
	for bucket in buckets {
		count += bucket.len
	}
	assert count == 5
}
