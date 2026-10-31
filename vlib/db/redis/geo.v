module redis

// GeoUnit selects the unit for geospatial distances and search dimensions.
pub enum GeoUnit {
	m
	km
	ft
	mi
}

// GeoPosition is a longitude and latitude pair in degrees.
pub struct GeoPosition {
pub:
	longitude f64
	latitude  f64
}

// GeoMember associates a member name with a geographic position.
pub struct GeoMember {
pub:
	name      string
	longitude f64
	latitude  f64
}

// GeoAddMode controls whether geoadd inserts new members or updates existing members.
pub enum GeoAddMode {
	all
	nx
	xx
}

// GeoAddOptions configures conditional updates and counting of changed members.
@[params]
pub struct GeoAddOptions {
pub:
	mode    GeoAddMode
	changed bool
}

// GeoOrder selects distance ordering for geospatial searches.
pub enum GeoOrder {
	unsorted
	asc
	desc
}

// GeoSearchOptions selects a circle or box centered on a member or longitude/latitude.
// Use radius for a circle, or width and height for a box. Optional result fields are explicit.
@[params]
pub struct GeoSearchOptions {
pub:
	from_member ?string
	longitude   f64
	latitude    f64
	radius      f64
	width       f64
	height      f64
	unit        GeoUnit = .m
	order       GeoOrder
	count       int
	any         bool
	with_dist   bool
	with_hash   bool
	with_coord  bool
}

// GeoSearchResult contains a member and the optional metadata requested by a search.
pub struct GeoSearchResult {
pub:
	member   string
	distance ?f64
	hash     ?i64
	position ?GeoPosition
}

fn geo_number(value RedisValue, command string) !f64 {
	match value {
		f64 { return value }
		f32 { return f64(value) }
		i64 { return f64(value) }
		else { return bulk_value[string](value, command)!.f64() }
	}
}

fn geo_position(value RedisValue, command string) !GeoPosition {
	coordinates := array_value(value, command)!
	if coordinates.len != 2 {
		return ProtocolError{ message: '`${command}()`: invalid coordinates' }
	}
	return GeoPosition{
		longitude: geo_number(coordinates[0], command)!
		latitude:  geo_number(coordinates[1], command)!
	}
}

// geoadd adds or updates positions and returns the number of new or changed members.
pub fn (mut db DB) geoadd(key string, members []GeoMember, options GeoAddOptions) !i64 {
	mut args := ['GEOADD', key]
	if options.mode != .all {
		args << options.mode.str().to_upper()
	}
	if options.changed {
		args << 'CH'
	}
	for member in members {
		args << member.longitude.str()
		args << member.latitude.str()
		args << member.name
	}
	return db.execute_i64(args)
}

// geodist returns the distance between members; a missing member returns an error.
pub fn (mut db DB) geodist(key string, first string, second string, unit GeoUnit) !f64 {
	return db.execute_f64(['GEODIST', key, first, second, unit.str()])
}

// geohash returns geohashes in member order, preserving missing members as none.
pub fn (mut db DB) geohash(key string, members ...string) ![]?string {
	mut args := ['GEOHASH', key]
	args << members
	return db.execute_nullable[string](args)
}

// geopos returns positions in member order, preserving missing members as none.
pub fn (mut db DB) geopos(key string, members ...string) ![]?GeoPosition {
	mut args := ['GEOPOS', key]
	args << members
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []?GeoPosition{}
	}
	values := array_value(resp, 'geopos')!
	mut result := []?GeoPosition{cap: values.len}
	for value in values {
		if value is RedisNull {
			result << none
		} else {
			result << ?GeoPosition(geo_position(value, 'geopos')!)
		}
	}
	return result
}

fn geo_search_args(args []string, options GeoSearchOptions, store bool) ![]string {
	if options.count < 0 || (options.any && options.count == 0) {
		return CommandError{ message: 'geospatial COUNT must be positive when ANY is requested' }
	}
	mut result := args.clone()
	if member := options.from_member {
		result << 'FROMMEMBER'
		result << member
	} else {
		result << 'FROMLONLAT'
		result << options.longitude.str()
		result << options.latitude.str()
	}
	if options.radius > 0 && options.width == 0 && options.height == 0 {
		result << 'BYRADIUS'
		result << options.radius.str()
	} else if options.radius == 0 && options.width > 0 && options.height > 0 {
		result << 'BYBOX'
		result << options.width.str()
		result << options.height.str()
	} else {
		return CommandError{ message: 'geospatial search requires a positive radius or positive width and height' }
	}
	result << options.unit.str()
	if options.order != .unsorted {
		result << options.order.str().to_upper()
	}
	if options.count > 0 {
		result << 'COUNT'
		result << options.count.str()
		if options.any {
			result << 'ANY'
		}
	}
	if store && (options.with_dist || options.with_hash || options.with_coord) {
		return CommandError{ message: '`geosearchstore()`: response metadata options cannot be stored' }
	}
	if options.with_dist {
		result << 'WITHDIST'
	}
	if options.with_hash {
		result << 'WITHHASH'
	}
	if options.with_coord {
		result << 'WITHCOORD'
	}
	return result
}

fn (mut db DB) execute_geo_search(args []string, options GeoSearchOptions) ![]GeoSearchResult {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []GeoSearchResult{}
	}
	values := array_value(resp, args[0].to_lower())!
	mut result := []GeoSearchResult{cap: values.len}
	for value in values {
		if !options.with_dist && !options.with_hash && !options.with_coord {
			result << GeoSearchResult{ member: bulk_value[string](value, 'geosearch')! }
			continue
		}
		parts := array_value(value, 'geosearch')!
		expected := 1 + (if options.with_dist { 1 } else { 0 }) + (if options.with_hash {
			1
		} else {
			0
		}) + (if options.with_coord { 1 } else { 0 })
		if parts.len != expected {
			return ProtocolError{ message: '`geosearch()`: invalid search response' }
		}
		mut index := 1
		mut distance := ?f64(none)
		mut hash := ?i64(none)
		mut position := ?GeoPosition(none)
		if options.with_dist {
			distance = geo_number(parts[index], 'geosearch')!
			index++
		}
		if options.with_hash {
			if parts[index] is i64 {
				hash = parts[index] as i64
			} else {
				return ProtocolError{ message: '`geosearch()`: invalid geohash response' }
			}
			index++
		}
		if options.with_coord {
			position = geo_position(parts[index], 'geosearch')!
		}
		result << GeoSearchResult{
			member:   bulk_value[string](parts[0], 'geosearch')!
			distance: distance
			hash:     hash
			position: position
		}
	}
	return result
}

// geosearch searches a geospatial index in a circle or box.
pub fn (mut db DB) geosearch(key string, options GeoSearchOptions) ![]GeoSearchResult {
	return db.execute_geo_search(geo_search_args(['GEOSEARCH', key], options, false)!, options)
}

// geosearchstore stores search results as a sorted set, optionally using distance as score.
pub fn (mut db DB) geosearchstore(destination string, source string, options GeoSearchOptions, store_dist bool) !i64 {
	mut args := geo_search_args(['GEOSEARCHSTORE', destination, source], options, true)!
	if store_dist {
		args << 'STOREDIST'
	}
	return db.execute_i64(args)
}

// georadius searches a circle centered on coordinates using the legacy Redis command.
// Prefer geosearch for new code. Options must describe a circle centered on coordinates.
pub fn (mut db DB) georadius(key string, options GeoSearchOptions) ![]GeoSearchResult {
	if options.from_member != none || options.radius <= 0 || options.width != 0 || options.height != 0 {
		return CommandError{ message: '`georadius()`: a coordinate-centered circle is required' }
	}
	search := geo_search_args(['GEOSEARCH', key], options, false)!
	mut args := ['GEORADIUS', key, options.longitude.str(), options.latitude.str(),
		options.radius.str(), options.unit.str()]
	args << search[8..]
	return db.execute_geo_search(args, options)
}

// georadiusbymember searches a circle centered on a member using the legacy Redis command.
// Prefer geosearch for new code. Options must describe a circle centered on a member.
pub fn (mut db DB) georadiusbymember(key string, options GeoSearchOptions) ![]GeoSearchResult {
	member := options.from_member or { return CommandError{ message: '`georadiusbymember()`: a center member is required' } }
	if options.radius <= 0 || options.width != 0 || options.height != 0 {
		return CommandError{ message: '`georadiusbymember()`: a positive circle radius is required' }
	}
	search := geo_search_args(['GEOSEARCH', key], options, false)!
	mut args := ['GEORADIUSBYMEMBER', key, member, options.radius.str(), options.unit.str()]
	args << search[7..]
	return db.execute_geo_search(args, options)
}
