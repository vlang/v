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
	match f {
		0    { return 'decimal'     }
		1    { return 'tiny'        }
		2    { return 'short'       }
		3    { return 'long'        }
		4    { return 'float'       }
		5    { return 'double'      }
		6    { return 'null'        }
		7    { return 'timestamp'   }
		8    { return 'longlong'    }
		9    { return 'int24'       }
		10   { return 'date'        }
		11   { return 'time'        }
		12   { return 'datetime'    }
		13   { return 'year'        }
		14   { return 'newdate'     }
		15   { return 'varchar'     }
		16   { return 'bit'         }
		17   { return 'timestamp2'  }
		18   { return 'datetime2'   }
		19   { return 'time2'       }
		245  { return 'json'        }
		246  { return 'newdecimal'  }
		247  { return 'enum'        }
		248  { return 'set'         }
		249  { return 'tiny_blob'   }
		250  { return 'medium_blob' }
		251  { return 'long_blob'   }
		252  { return 'blob'        }
		253  { return 'var_string'  }
		254  { return 'string'      }
		255  { return 'geometry'    }
		else { return 'unknown'     }
	}
}
