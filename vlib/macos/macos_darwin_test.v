module macos

$if macos {
	#flag darwin -framework AppKit

	fn test_point_and_range_constructors() {
		p := point(12.5, -3.25)
		assert p.x == 12.5
		assert p.y == -3.25
		r := range(4, 7)
		assert r.location == 4
		assert r.length == 7
	}

	fn test_typed_range_message_senders() {
		pool := autorelease_pool_new()
		defer {
			release(pool)
		}
		value := msg_id_range(get_class('NSValue'), 'valueWithRange:', range(2, 5))
		result := msg_range(value, 'rangeValue')
		assert result.location == 2
		assert result.length == 5

		text := msg_id_range(nsstring('hello'), 'substringWithRange:', range(1, 3))
		assert utf8_string(text) == 'ell'
		assert responds_to(text, 'substringWithRange:')
		assert msg_bool_id(text, 'isEqualToString:', nsstring('ell'))
	}

	fn test_typed_point_message_senders() {
		pool := autorelease_pool_new()
		defer {
			release(pool)
		}
		shadow := new('NSShadow')
		defer {
			release(shadow)
		}
		msg_void_point(shadow, 'setShadowOffset:', point(1.5, -2.5))
		result := msg_point(shadow, 'shadowOffset')
		assert result.x == 1.5
		assert result.y == -2.5
	}
}
