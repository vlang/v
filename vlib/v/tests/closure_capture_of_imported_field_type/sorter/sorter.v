module sorter

import owner

// order captures a local whose type comes from the `owner.Log.rows` field,
// declared there with the bare spelling `[]Row`.
pub fn order(log owner.Log) []int {
	rows := log.rows
	mut idx := []int{len: rows.len, init: index}
	idx.sort_with_compare(fn [rows] (a &int, b &int) int {
		ta := rows[*a].t_s
		tb := rows[*b].t_s
		return if ta < tb {
			-1
		} else if ta > tb {
			1
		} else {
			0
		}
	})
	return idx
}
