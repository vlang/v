// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Structure of Arrays (SoA) code generation.
//
// When a struct is annotated with @[soa], the compiler generates a companion
// SoA container struct that stores each field in a separate contiguous array.
// This layout provides significantly better cache performance for batch
// operations that touch only a subset of fields (common in game math, ECS,
// particle systems, physics simulations, etc.).
//
// Example:
//   @[soa]
//   struct Particle {
//       x  f32
//       y  f32
//       vx f32
//       vy f32
//   }
//
// Generates a companion type `Particle_SOA` with:
//   struct Particle_SOA {
//       int len;
//       int cap;
//       f32* x;
//       f32* y;
//       f32* vx;
//       f32* vy;
//   };
//
// And helper functions:
//   Particle_SOA Particle_SOA_new(int len, int cap);
//   void Particle_SOA_push(Particle_SOA* soa, Particle val);
//   Particle Particle_SOA_get(Particle_SOA soa, int i);
//   void Particle_SOA_set(Particle_SOA* soa, int i, Particle val);
//   void Particle_SOA_free(Particle_SOA* soa);
module cleanc

import v2.types

// gen_soa_companion generates the SoA container struct and helper functions
// for a struct annotated with @[soa].
fn (mut g Gen) gen_soa_companion(name string, s types.Struct) {
	soa_name := '${name}_SOA'

	// --- SoA container struct ---
	g.sb.writeln('// SoA (Structure of Arrays) companion for ${name}')
	g.sb.writeln('typedef struct {')
	g.sb.writeln('\tint len;')
	g.sb.writeln('\tint cap;')
	for field in s.fields {
		c_type := g.types_type_to_c(field.typ)
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\t${c_type}* ${fname};')
	}
	g.sb.writeln('} ${soa_name};')
	g.sb.writeln('')

	// --- new: allocate SoA container ---
	g.sb.writeln('static inline ${soa_name} ${soa_name}_new(int len, int cap) {')
	g.sb.writeln('\tif (cap < len) cap = len;')
	g.sb.writeln('\t${soa_name} soa;')
	g.sb.writeln('\tsoa.len = len;')
	g.sb.writeln('\tsoa.cap = cap;')
	for field in s.fields {
		c_type := g.types_type_to_c(field.typ)
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\tsoa.${fname} = (${c_type}*)calloc(cap, sizeof(${c_type}));')
	}
	g.sb.writeln('\treturn soa;')
	g.sb.writeln('}')
	g.sb.writeln('')

	// --- get: retrieve element at index as original struct ---
	g.sb.writeln('static inline ${name} ${soa_name}_get(${soa_name} soa, int i) {')
	g.sb.writeln('\treturn (${name}){')
	for i, field in s.fields {
		comma := if i < s.fields.len - 1 { ',' } else { '' }
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\t\t.${fname} = soa.${fname}[i]${comma}')
	}
	g.sb.writeln('\t};')
	g.sb.writeln('}')
	g.sb.writeln('')

	// --- set: set element at index from original struct ---
	g.sb.writeln('static inline void ${soa_name}_set(${soa_name}* soa, int i, ${name} val) {')
	for field in s.fields {
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\tsoa->${fname}[i] = val.${fname};')
	}
	g.sb.writeln('}')
	g.sb.writeln('')

	// --- push: append element, growing capacity if needed ---
	g.sb.writeln('static inline void ${soa_name}_push(${soa_name}* soa, ${name} val) {')
	g.sb.writeln('\tif (soa->len >= soa->cap) {')
	g.sb.writeln('\t\tint new_cap = soa->cap < 8 ? 8 : soa->cap * 2;')
	for field in s.fields {
		c_type := g.types_type_to_c(field.typ)
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\t\tsoa->${fname} = (${c_type}*)realloc(soa->${fname}, new_cap * sizeof(${c_type}));')
	}
	g.sb.writeln('\t\tsoa->cap = new_cap;')
	g.sb.writeln('\t}')
	for field in s.fields {
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\tsoa->${fname}[soa->len] = val.${fname};')
	}
	g.sb.writeln('\tsoa->len++;')
	g.sb.writeln('}')
	g.sb.writeln('')

	// --- pop: remove and return last element ---
	g.sb.writeln('static inline ${name} ${soa_name}_pop(${soa_name}* soa) {')
	g.sb.writeln('\tif (soa->len == 0) return (${name}){0};')
	g.sb.writeln('\tsoa->len--;')
	g.sb.writeln('\treturn (${name}){')
	for i, field in s.fields {
		comma := if i < s.fields.len - 1 { ',' } else { '' }
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\t\t.${fname} = soa->${fname}[soa->len]${comma}')
	}
	g.sb.writeln('\t};')
	g.sb.writeln('}')
	g.sb.writeln('')

	// --- free: deallocate all field arrays ---
	g.sb.writeln('static inline void ${soa_name}_free(${soa_name}* soa) {')
	for field in s.fields {
		fname := escape_c_keyword(field.name)
		g.sb.writeln('\tfree(soa->${fname});')
	}
	g.sb.writeln('\tsoa->len = 0;')
	g.sb.writeln('\tsoa->cap = 0;')
	g.sb.writeln('}')
	g.sb.writeln('')
}
