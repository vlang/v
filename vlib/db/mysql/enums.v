module mysql

pub enum FieldType {
	type_decimal
	type_tiny
	type_short
	type_long
	type_float
	type_double
	type_null
	type_timestamp
	type_longlong
	type_int24
	type_date
	type_time
	type_datetime
	type_year
	type_newdate
	type_varchar
	type_bit
	type_timestamp2
	type_datetime2
	type_time2
	type_json        = 245
	type_newdecimal
	type_enum
	type_set
	type_tiny_blob
	type_medium_blob
	type_long_blob
	type_blob
	type_var_string
	type_string
	type_geometry
}

pub fn (f FieldType) str() string {
	return match f {
		0    { 'decimal'     }
		1    { 'tiny'        }
		2    { 'short'       }
		3    { 'long'        }
		4    { 'float'       }
		5    { 'double'      }
		6    { 'null'        }
		7    { 'timestamp'   }
		8    { 'longlong'    }
		9    { 'int24'       }
		10   { 'date'        }
		11   { 'time'        }
		12   { 'datetime'    }
		13   { 'year'        }
		14   { 'newdate'     }
		15   { 'varchar'     }
		16   { 'bit'         }
		17   { 'timestamp2'  }
		18   { 'datetime2'   }
		19   { 'time2'       }
		245  { 'json'        }
		246  { 'newdecimal'  }
		247  { 'enum'        }
		248  { 'set'         }
		249  { 'tiny_blob'   }
		250  { 'medium_blob' }
		251  { 'long_blob'   }
		252  { 'blob'        }
		253  { 'var_string'  }
		254  { 'string'      }
		255  { 'geometry'    }
		else { 'unknown'     }
	}
}
