module types

import v.flat

// Constant type spellings that checker functions compare against; each is interned once.
const checker_source_array_flags = flat.source_type_id('ArrayFlags')
const checker_source_atomic = flat.source_type_id('atomic')
const checker_source_builtin = flat.source_type_id('builtin')
const checker_source_c = flat.source_type_id('C')
const checker_source_chan_state = flat.source_type_id('ChanState')
const checker_source_context = flat.source_type_id('Context')
const checker_source_iclone = flat.source_type_id('IClone')
const checker_source_mut_context = flat.source_type_id('mut Context')
const checker_source_option_f64 = flat.source_type_id('?f64')
const checker_source_option_int = flat.source_type_id('?int')
const checker_source_option_string = flat.source_type_id('?string')
const checker_source_option_time_time = flat.source_type_id('?time.Time')
const checker_source_orm_connection = flat.source_type_id('orm.Connection')
const checker_source_result = flat.source_type_id('Result')
const checker_source_shared = flat.source_type_id('shared')
const checker_source_space = flat.source_type_id(' ')
const checker_source_u8_reference = flat.source_type_id('&u8')
const checker_source_v3_comptime_type_array = flat.source_type_id('__v3_comptime_type_array')
const checker_source_v3_prewarm = flat.source_type_id('__V3Prewarm__')
const checker_source_veb_middleware = flat.source_type_id('veb.Middleware')
const checker_source_vmap_data = flat.source_type_id('VMapData')
