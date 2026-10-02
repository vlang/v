module redis

// BitUnit selects the unit used by a bitmap range.
pub enum BitUnit {
	byte
	bit
}

// BitRange configures an inclusive byte or bit range for bitcount and bitpos.
@[params]
pub struct BitRange {
pub:
	start ?i64
	end   ?i64
	unit  BitUnit
}

// BitOperation selects a bitwise operation for bitop.
pub enum BitOperation {
	and
	or
	xor
	not
}

// BitFieldKind selects the operation performed on a bitfield.
pub enum BitFieldKind {
	get
	set
	incrby
}

// BitOverflow selects bitfield overflow behavior; default emits no OVERFLOW directive.
pub enum BitOverflow {
	default
	wrap
	sat
	fail
}

// BitFieldOperation addresses a signed or unsigned integer at a bit offset.
// Encoding uses Redis syntax, such as i16 or u8. Offset also accepts indexed offsets like #2.
pub struct BitFieldOperation {
pub:
	kind     BitFieldKind
	encoding string
	offset   string
	value    i64
	overflow BitOverflow
}

fn bitmap_range_args(args []string, options BitRange) ![]string {
	mut result := args.clone()
	if start := options.start {
		result << start.str()
		if end := options.end {
			result << end.str()
			if options.unit == .bit {
				result << 'BIT'
			}
		} else if options.unit == .bit {
			return CommandError{ message: 'bit ranges require both start and end' }
		}
	} else if options.end != none || options.unit == .bit {
		return CommandError{ message: 'bitmap range end and unit require a start' }
	}
	return result
}

// setbit sets a bit and returns its previous value.
pub fn (mut db DB) setbit(key string, offset i64, value bool) !bool {
	return db.execute_i64(['SETBIT', key, offset.str(), if value { '1' } else { '0' }])! != 0
}

// getbit retrieves a bit; bits beyond the value's length are false.
pub fn (mut db DB) getbit(key string, offset i64) !bool {
	return db.execute_i64(['GETBIT', key, offset.str()])! != 0
}

// bitcount counts set bits in a string or an inclusive byte or bit range.
pub fn (mut db DB) bitcount(key string, options BitRange) !i64 {
	if options.start != none && options.end == none {
		return CommandError{ message: '`bitcount()`: range requires both start and end' }
	}
	return db.execute_i64(bitmap_range_args(['BITCOUNT', key], options)!)
}

// bitpos finds the first matching bit; -1 means no matching bit was found.
pub fn (mut db DB) bitpos(key string, value bool, options BitRange) !i64 {
	return db.execute_i64(bitmap_range_args(['BITPOS', key, if value { '1' } else { '0' }], options)!)
}

// bitop stores a bitwise operation over source strings and returns the destination length.
pub fn (mut db DB) bitop(operation BitOperation, destination string, keys ...string) !i64 {
	if operation == .not && keys.len != 1 {
		return CommandError{ message: '`bitop()`: NOT requires exactly one source key' }
	}
	mut args := ['BITOP', operation.str().to_upper(), destination]
	args << keys
	return db.execute_i64(args)
}

fn bitfield_args(command string, key string, operations []BitFieldOperation) ![]string {
	mut args := [command, key]
	for operation in operations {
		if command == 'BITFIELD_RO' && (operation.kind != .get || operation.overflow != .default) {
			return CommandError{ message: '`bitfield_ro()`: only GET operations are allowed' }
		}
		if operation.overflow != .default {
			args << 'OVERFLOW'
			args << operation.overflow.str().to_upper()
		}
		args << operation.kind.str().to_upper()
		args << operation.encoding
		args << operation.offset
		if operation.kind != .get {
			args << operation.value.str()
		}
	}
	return args
}

fn (mut db DB) execute_bitfield(args []string) ![]?i64 {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []?i64{}
	}
	values := array_value(resp, args[0].to_lower())!
	mut result := []?i64{cap: values.len}
	for value in values {
		match value {
			i64 { result << ?i64(value) }
			RedisNull { result << none }
			else {
				return ProtocolError{ message: '`${args[0].to_lower()}()`: invalid bitfield response' }
			}
		}
	}
	return result
}

// bitfield performs integer operations on bitfields; failed overflow operations return none.
pub fn (mut db DB) bitfield(key string, operations ...BitFieldOperation) ![]?i64 {
	return db.execute_bitfield(bitfield_args('BITFIELD', key, operations)!)
}

// bitfield_ro reads bitfields without modifying the key.
pub fn (mut db DB) bitfield_ro(key string, operations ...BitFieldOperation) ![]?i64 {
	return db.execute_bitfield(bitfield_args('BITFIELD_RO', key, operations)!)
}

// pfadd adds elements to a HyperLogLog and reports whether its registers changed.
pub fn (mut db DB) pfadd(key string, elements ...string) !bool {
	mut args := ['PFADD', key]
	args << elements
	return db.execute_i64(args)! != 0
}

// pfcount estimates the cardinality of the union of HyperLogLogs.
pub fn (mut db DB) pfcount(keys ...string) !i64 {
	mut args := ['PFCOUNT']
	args << keys
	return db.execute_i64(args)
}

// pfmerge stores the union of source HyperLogLogs in destination.
pub fn (mut db DB) pfmerge(destination string, keys ...string) !string {
	mut args := ['PFMERGE', destination]
	args << keys
	return db.execute_string(args)
}
