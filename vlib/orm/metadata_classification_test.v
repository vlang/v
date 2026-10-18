module orm

import time

type MetadataInt = int

type MetadataTime = time.Time

enum MetadataStatus {
	first
	second
}

struct MetadataEmbedded {
	enabled bool
	ratio   ?f64
}

struct MetadataRelation {
	id int
}

struct MetadataModel {
	MetadataEmbedded
	id            int @[primary]
	alias         MetadataInt
	optional      ?MetadataInt
	created       MetadataTime
	deleted       ?MetadataTime
	status        MetadataStatus
	optional_enum ?MetadataStatus
	text          ?string
	relation      MetadataRelation
	skipped       string @[sql: '-']
}

fn test_metadata_classification_preserves_embedded_optional_and_alias_types() {
	meta := struct_meta[MetadataModel]()
	expected := {
		'MetadataEmbedded.enabled': type_idx['bool']
		'MetadataEmbedded.ratio':   type_idx['f64']
		'id':                       type_idx['int']
		'alias':                    type_idx['int']
		'optional':                 type_idx['int']
		'created':                  time_
		'deleted':                  time_
		'status':                   enum_
		'optional_enum':            enum_
		'text':                     type_string
		'relation':                 type_idx['int']
	}
	assert meta.len == expected.len
	for field in meta {
		assert field.name in expected, field.name
		assert field.typ == expected[field.name], field.name
		assert field.nullable == (field.name in ['MetadataEmbedded.ratio', 'optional', 'deleted',
			'optional_enum', 'text']), field.name
	}
}

struct MetadataArrays {
	values          []string
	optional_values ?[]string
	skipped         string @[skip]
}

fn test_metadata_preserves_array_and_attribute_flags() {
	arrays := struct_meta[MetadataArrays]()
	assert arrays.map(it.name) == ['values', 'optional_values']
	assert arrays.all(it.is_arr)
	assert !arrays[0].nullable
	assert arrays[1].nullable
	model := struct_meta[MetadataModel]()
	id := model.filter(it.name == 'id')[0]
	assert id.attrs.any(it.name == 'primary')
}
