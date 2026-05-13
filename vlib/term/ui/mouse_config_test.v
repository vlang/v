module ui

fn test_mouse_tracking_is_disabled_by_default() {
	cfg := Config{}
	assert !cfg.mouse_enabled
}

fn test_mouse_tracking_can_be_enabled_explicitly() {
	cfg := Config{
		mouse_enabled: true
	}
	assert cfg.mouse_enabled
}
