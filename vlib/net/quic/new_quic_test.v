module quic

fn test_ngtcp2_bindings() {
	println('Testing ngtcp2 struct sizes and initialization...')
	
	settings := Ngtcp2SettingsStruct{
		token: unsafe { nil }
		preferred_versions: unsafe { nil }
		available_versions: unsafe { nil }
		pmtud_probes: unsafe { nil }
	}
	assert sizeof(settings) > 0
	
	params := Ngtcp2TransportParamsStruct{
		version_info: Ngtcp2VersionInfo{
			available_versions: unsafe { nil }
		}
	}
	assert sizeof(params) > 0

	println('ngtcp2 bindings test passed')
}

fn test_connection_config() {
	println('Testing ConnectionConfig...')
	config := ConnectionConfig{
		remote_addr: '127.0.0.1:4433'
		alpn: ['h3']
		max_idle_timeout: 30000
	}
	assert config.alpn.len == 1
	assert config.alpn[0] == 'h3'
	assert config.max_idle_timeout == 30000
}

fn test_zero_rtt_structures() {
    println('Testing 0-RTT structures...')
    
    config := ZeroRTTConfig{
        enabled: true
        max_early_data: 8192
    }
    assert config.enabled == true
    assert config.max_early_data == 8192
}

fn test_config_validation() {
	println('Testing QUIC config validation...')
	default_config := ConnectionConfig{
		remote_addr: '127.0.0.1:4433'
	}
	// Check defaults
	assert default_config.max_idle_timeout == 30000
    assert default_config.max_stream_data_bidi_local == 1048576
    assert default_config.max_streams_bidi == 100
    
    // Check ALPN
    assert 'h3' in default_config.alpn
}
