fn result_void_short_circuit(race bool, defines []string) ! {
	if !race && defines.any(it == 'race') {
		return error('reserved define')
	}
}

fn result_void_bare_return(stop bool) ! {
	if stop {
		return
	}
}

fn test_result_void_success_returns_from_empty_short_circuit_and_bare_return() {
	result_void_short_circuit(false, [])!
	result_void_short_circuit(true, ['race'])!
	result_void_bare_return(true)!
	result_void_bare_return(false)!
}
