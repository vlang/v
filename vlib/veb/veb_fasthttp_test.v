module veb

fn test_listening_process_ids_from_lsof_output() {
	assert listening_process_ids_from_lsof_output('42\n123\n') == [42, 123]
	assert listening_process_ids_from_lsof_output('42\ninvalid\n0\n-1\n') == [42]
}
