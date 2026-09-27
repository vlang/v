module bench

fn current_limit_memory() LimitMemory {
	return LimitMemory{
		kb:     current_rss_kb()
		metric: 'RSS'
	}
}
