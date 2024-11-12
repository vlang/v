module asn1

struct StringOption {
	src         string
	cls         string
	tagnum      int
	mode        string
	inner       string
	optional    bool
	has_default bool
	err         IError
}

fn test_parse_string_option() ! {
	data := [
		// should parseable
		StringOption{'application:20;explicit;inner:5', 'application', 20, 'explicit', '5', false, false, none},
		StringOption{'private:0x20;implicit;inner:5', 'private', 32, 'implicit', '5', false, false, none},
		StringOption{'context_specific:0x20;implicit;inner:5', 'context_specific', 32, 'implicit', '5', false, false, none},
		StringOption{'private:0x20;implicit;inner:5; optional', 'private', 32, 'implicit', '5', true, false, none},
		StringOption{'private:0x20;implicit;inner:5; has_default', 'private', 32, 'implicit', '5', false, true, none},
		StringOption{'private:0x20;implicit;inner:5; optional; has_default', 'private', 32, 'implicit', '5', true, true, none},
		// not parseable
		// Without mode or inner
		StringOption{'application:20', 'application', 0, '', '5', false, false, error('Invalid zonk or uncorerct mode value')},
		StringOption{'application:20; inner:4', 'application', 0, '', '0', false, false, error('Invalid zonk or uncorerct mode value')},
		StringOption{'application:20; implicit', 'application', 0, '', '0', false, false, error('You provides incorrect inner number')},
		StringOption{'implicit;inner:33', '', -1, 'implicit', '33', false, false, error('You provides incorrect inner number')},
	]
	for i, item in data {
		// dump(i)
		fo := FieldOptions.from_string(item.src) or {
			assert err == item.err
			continue
		}
		assert fo.cls == item.cls
		assert fo.tagnum == item.tagnum
		assert fo.optional == item.optional
		assert fo.has_default == item.has_default
		assert fo.mode == item.mode
		assert fo.inner == item.inner
	}
}

struct TagMarker {
	attr string
	cls  string
	num  int
	err  IError
}

fn test_tag_marker_parsing() ! {
	data := [
		// normal
		TagMarker{'application:100', 'application', 100, none},
		TagMarker{'context_specific:100', 'context_specific', 100, none},
		// normal with hex number
		TagMarker{'private:0x54', 'private', 0x54, none},
		// normal with spaces should be allowed
		TagMarker{'application: 0x20', 'application', 0x20, none},
		TagMarker{'    application   : 0x20    ', 'application', 0x20, none},
		// universal should not allowed
		TagMarker{'universal:0x5f', 'universal', 0x5f, error('not a tag marker')},
		// bad tag key should error
		TagMarker{'embuh: 0x20', '', 0x20, error('not a tag marker')},
		TagMarker{'private_embuh: 0x20', '', 0x20, error('bad tag name')},
		// key without number also error
		TagMarker{'private_embuh', '', 0, error('bad tag marker length')},
		TagMarker{'private:', 'private', 0, error('bad tag number')},
		TagMarker{'private:bb', 'private', 0, error('bad tag number')},
	]
	for item in data {
		k, v := parse_tag_marker(item.attr) or {
			assert err == item.err
			continue
		}
		assert k == item.cls
		assert v.int() == item.num
	}
}

struct ModeMarker {
	attr  string
	value string
	err   IError
}

fn test_mode_marker_parsing() ! {
	data := [
		// the normal right thing
		ModeMarker{'explicit', 'explicit', none},
		ModeMarker{'implicit', 'implicit', none},
		// with spaces is allowed
		ModeMarker{'    implicit ', 'implicit', none},
		ModeMarker{'    explicit    ', 'explicit', none},
		// bad key or value
		ModeMarker{'xx_implicit', '', error('not mode marker')},
		ModeMarker{'implicitkey', '', error('bad mode value')},
		ModeMarker{'exoplicit implicit', '', error('not mode marker')},
	]
	for i, item in data {
		// dump(i)
		v := parse_mode_marker(item.attr) or {
			assert err == item.err
			continue
		}
		assert valid_mode_value(v) == true
		assert v == item.value
	}
}

struct InnerMarker {
	src    string
	result string
	err    IError
}

fn test_for_inner_tag_marker() ! {
	data := [InnerMarker{'', '', error('not inner tag marker')},
		InnerMarker{'inner:0', '0', none}, InnerMarker{'inner:12', '12', none},
		InnerMarker{'inner:private,true,14', 'private,true,14', none},
		InnerMarker{'inner:universal,true,14', 'universal,true,14', none},
		InnerMarker{'inner:invalid,true,14', '', error('Your first ext inner is not extended cls')}]
	for i, item in data {
		// dump(i)
		k, v := parse_inner_tag_marker(item.src) or {
			assert err == item.err
			continue
		}

		assert v == item.result
	}
}

struct HasDefaultMarker {
	attr string
	err  IError
}

fn test_has_default_marker_parsing() ! {
	data := [
		HasDefaultMarker{'has_default', none},
		HasDefaultMarker{'  has_default  ', none},
		HasDefaultMarker{'', error('not has_default marker')},
		HasDefaultMarker{'has_default:', error('bad has_default marker')},
		HasDefaultMarker{'has_defaultaa', error('bad has_default marker')},
	]
	for item in data {
		s := parse_default_marker(item.attr) or {
			assert err == item.err
			continue
		}
		assert valid_default_marker(s) == true
	}
}

struct OptionalMarker {
	attr    string
	valid   bool
	present bool
	err     IError
}

fn test_optional_marker_parsing() ! {
	data := [
		// exactly matching key
		OptionalMarker{'optional', true, false, none},
		OptionalMarker{'optional:    present ', true, true, none},
		// matching key contains spaces is allowed
		OptionalMarker{'optional ', true, false, none},
		OptionalMarker{'      optional ', true, false, none},
		OptionalMarker{'optional:present ', true, true, none},
		// contains another key is not allowed
		OptionalMarker{'optional: true ', false, false, error('Non optional presence bit marker')},
		OptionalMarker{'optional-- ', false, false, error('bad optional key')},
		// this should not allowed
		OptionalMarker{'', false, false, error('not optional marker')},
		OptionalMarker{'optional_aaa', false, false, error('bad optional key')},
		OptionalMarker{'opt', false, false, error('not optional marker')},
		OptionalMarker{'xx_optional_ ', false, false, error('not optional marker')},
	]
	for i, item in data {
		// dump(i)
		key, prs := parse_optional_marker(item.attr) or {
			assert err == item.err
			continue
		}
		assert valid_optional_key(key) == item.valid
		assert valid_optional_present_bit_marker(prs) == item.present
	}
}
