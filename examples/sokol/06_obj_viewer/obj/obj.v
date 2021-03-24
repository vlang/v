/**********************************************************************
*
* .obj loader
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
module obj
import gg.m4
import strconv

enum F_state{
	start
	first
	ints
	decimals
	exp_start
	exp_sign
	exp_int
}

// read a int from a string
fn get_int(s string, start_index int) (int, int) {
	mut i     := start_index
	mut res   := 0
	mut sgn   := 1
	
	mut state := F_state.start
	for true {
		if i >= s.len {
			break
		}
		c := s[i]
		if state == .start {
			match c {
				`+` {
					i++
					state = .ints
					continue
				}
				`-` {
					sgn = -1
					i++
					state = .ints
					continue
				}
				`0`...`9` {
					state = .ints
				}
				` `,`\t` {
					i++
					continue
				}
				else{ // no number found
					break
				}
			}
		}
				
		if state == .ints {
			match c {
				`0`...`9` {
					//println("$res => ${(int(c) - 48)}")
					res = res * 10 + (int(c) - 48)
					i++
					continue
				}
				else {
					break
				}
			}
		}
		
	}
	//println("---")
	return res * sgn, i
}

// reas a float number from a string
fn get_float(s string, start_index int) (f64, int) {
	mut i1 := start_index //+ 1
	for i1 < s.len && s[i1] in [` `,`\t`] {
		i1++
	}
	mut i := i1
	for i < s.len {
		if s[i] in [` `,`\t`] {
			break
		}
		i++
	}	
	//println(" get_float: ($start_index,$i) [${s[start_index..i]}]")
  //f_res := strconv.atof_quick(s[start_index..i])
	f_res := strconv.atof_quick(s[i1..i])
	return f_res, i
}

// read 3 f32 in sequence from a string
fn parse_3f(row string, start_index int) m4.Vec4 {
	//println(row)
	mut i := start_index //+ 1
	mut f1 := f64(0)
	mut f2 := f64(0)
	f0,mut p := get_float(row,i)
	//print("Here f0: $f0 $p ")
	f1, p = get_float(row,p+1)
	//print("Here f1: $f1 $p ")
	f2, p = get_float(row,p+1)
	//print("Here f2: $f2 $p ")
	return m4.Vec4{e:[f32(f0), f32(f1), f32(f2), 1]!}
}

// reas a sequence of f32 from a string
fn (mut m ObjPart) parse_floats(row string, start_index int) m4.Vec4 {
	mut i       := start_index //+ 1
	mut res_f   := f64(0)
	mut res     := m4.Vec4{e:[f32(0), 0, 0, 1]!}
	mut c       := 0
	for true {
		res_f, i = get_float(row, i)
		unsafe { res.e[c] = f32(res_f) }
		c++
		i++
		if i >= row.len {
			break
		}
	}
	return res
}

// read and manage all the faes from an .obj file data
fn (mut p Part) parse_faces(row string, start_index int, obj ObjPart) {
	mut i       := start_index + 1
	mut res     := [][3]int{}
	mut v       := 0
	mut t       := 0
	mut n       := 0
	//println("row: ${row[i..]}")
	for true {
		t = 0
		n = 0
		if i >= row.len {
			break
		}
		mut c := row[i]
		if (c > `9` || c < `0`) && c != `-`{
			i++
			continue
		}
		v, i = get_int(row, i)
		if i < row.len && row[i] == `/` {
			if row[i+1] != `/` {
				t,i = get_int(row, i+1)
				if i < row.len && row[i] == `/` {
					n,i = get_int(row, i+1)
				}
			} else {
				i++
				n,i = get_int(row, i+1)
			}
			
		}
		// manage negative indexes
		// NOTE: not well suporeted now
		if v < 0 {
			//println("${obj.v.len} ${obj.v.len-c}")
			v = obj.v.len - v + 1
			//exit(0)
		}
		if n < 0 {
			n = obj.vn.len - n + 1
		}
		if t < 0 {
			t = obj.vt.len - t + 1
		}
		res << [v-1,n-1,t-1]!
	}
	//println("ok res: ${res}")
	//println(p.faces.len)
	p.faces << res
	
}

// parse the obj file, if single_material is true it use only one default material
pub fn (mut obj_part ObjPart) parse_obj_buffer(rows []string, single_material bool){
	mut mat_count := 0
	mut row_count := 0
	default_part := Part{name:"default part"}
	obj_part.part << default_part
	//println("OBJ file has ${rows.len} rows")
	for c, row in rows {
		//println("$c $row")
		mut i := 0
		row_count++
		for true {
			if i >= row.len {
				break
			}
			match row[i] {
				`s` {
					break
				}
				`m` {
					if row[i..i+6] == "mtllib" {
						obj_part.material_file = row[i+7..].trim_space()
						obj_part.load_materials()
					}
					break
				}
				`o`, `g` {
					mut part := Part{}
					part.name = row[i+1..].trim_space()
					obj_part.part << part
					mat_count = 0
					break
				}
				`u` {
					if single_material == false && row[i..i+6] == "usemtl" {
						material := row[i+7..].trim_space()
						//println("material: $material")
						// manage multiple materials in an part
						if obj_part.part[obj_part.part.len - 1].material.len > 0 {
							mat_count++
							mut part := Part{}
							if mat_count > 1 {
								li := obj_part.part[obj_part.part.len - 1].name.last_index("_m") or {obj_part.part[obj_part.part.len - 1].name.len - 1}
								part.name = obj_part.part[obj_part.part.len - 1].name[..li] + "_m${mat_count:02}"
							} else {
								part.name = obj_part.part[obj_part.part.len - 1].name + "_m01"
							}
							obj_part.part << part
						}
						obj_part.part[obj_part.part.len - 1].material = material
					}
					break
				}
				`v` {
					i++
					match row[i]{
						// normals
						`n` {
							obj_part.vn << parse_3f(row, i+2)
							//println("Vertex line: $c")
							break
						}
						// parameteres uvw
						`p` {
							obj_part.vp << parse_3f(row, i+2)
							//println("Vertex line: ${obj_part.vp.len}")
							break
						}
						// texture uvw
						`t` {
							obj_part.vt << obj_part.parse_floats(row, i+2)
							//println("Vertex line: $c")
							break
						}
						
						else {
							obj_part.v << parse_3f(row, i+1)
							//println("$row => ${obj_part.v[obj_part.v.len-1]}")
							break
						}
					}
				}
				`f` {
					//println("$c $row")
					obj_part.part[obj_part.part.len - 1].parse_faces(row, i, obj_part)
					//println(obj_part.part[obj_part.part.len - 1].faces.len)
					//println("Faces line: $c")
					break
				}
				// end of the line, comments
				`\n`,`#` {
					break
				}
				else{}
			}
			i++
		}
		//if c == 2 { break }
		if c % 100000 == 0 && c > 0{
			println("$c rows parsed")
		}
	}
	println("$row_count .obj Rows parsed")
	// remove default part if empty
	if obj_part.part.len > 1 && obj_part.part[0].faces.len == 0 {
		obj_part.part = obj_part.part[1..]
	}
}

// load the materials if found the .mtl file
fn (mut obj_part ObjPart) load_materials() {
	rows := obj.read_lines_from_file(obj_part.material_file)
	println("Material file [${obj_part.material_file}] ${rows.len} Rows.")
	for row in rows {
		//println("$row")
		mut i := 0
		for true {
			if i >= row.len {
				break
			}
			match row[i] {
				`n` {
					if row[i..i+6] == "newmtl" {
						name := row[i+6..].trim_space()
						mut mat := Material{name: name} 
						obj_part.mat << mat
						break
					}
				}
				`K` {
					if row[i+1] !in [`a`, `d`, `e`, `s`] {
						break
					}
					k_name := row[i..i+2]
					i += 3
					value := parse_3f(row, i)
					obj_part.mat[obj_part.mat.len - 1].ks[k_name] = value
					break
				}
				`N` {
					n_name := row[i..i+2]
					i += 3
					value, _ := get_float(row, i)
					obj_part.mat[obj_part.mat.len - 1].ns[n_name] = f32(value)
					break
				}
				`m` {
					if row[i..i+4] == "map_" {
						name := row[i..i+6]
						if (i + 7) < row.len {
							file_name := row[i+7..].trim_space()
							obj_part.mat[obj_part.mat.len - 1].maps[name] = file_name
						}
						break
					}
				}
				// trasparency
				`d` {
					if row[i+1] == ` ` {
						value, _ := get_float(row, i+2)
						obj_part.mat[obj_part.mat.len - 1].ns['Tr'] = f32(value)
					}
				}
				`T` {
					if row[i+1] == `r` {
						value, _ := get_float(row, i+3)
						obj_part.mat[obj_part.mat.len - 1].ns['Tr'] = f32(1.0 - value)
					}
				}
				// end of the line, comments
				`\n`,`#` {
					break
				}
				` `,`\t` {
					i++
					continue
				}
				else{
					break
				}
			}
			i++
		}
	}
	
	// create map material name => material index
	for i, m in obj_part.mat {
		if m.name !in obj_part.mat_map {
			obj_part.mat_map[m.name] = i
		}
	}

	println("Material Loading Done!")
}

//==============================================================================
// Sokol data
//==============================================================================

// vertex data struct
pub struct Vertex_pnct {
pub mut:
	x     f32 // poistion
	y     f32
	z     f32
	nx    f32 // normal
	ny    f32
	nz    f32
	color u32  = 0xFFFFFFFF // color
	u     f32 // uv
	v     f32
	// u u16   // for compatibility with D3D11
	// v u16   // for compatibility with D3D11
}

// struct used to pass the data to the sokol calls
pub struct Skl_buffer {
pub mut:
	vbuf []Vertex_pnct
	ibuf []u32
	n_vertex u32
}

// transforms data from .obj format to buffer ready to be used in the render
pub fn (mut obj_part ObjPart) get_buffer(in_part_list []int) Skl_buffer {
	//in_part           := 0
	mut v_count_index := 0
	mut out_buf       := Skl_buffer{}
	
	mut cache := map[string]int
	mut cache_hit := 0
	
	//has_normals := obj_part.vn.len > 0
	//has_uvs     := obj_part.vt.len > 0
	
	for in_part in in_part_list {
		part := obj_part.part[in_part]
		for fc, face in part.faces {
			//println("$fc $face")
			// default 3 faces
			mut v_seq := [0, 1, 2]
			if face.len == 4 {
				v_seq = [0, 1, 2, 0, 2, 3]
			}
			
			// if big faces => use the fan of triangles as solution
			// Note: this trick doesn't work with concave faces
			if face.len > 4 {
				v_seq = []
				mut i := 1
				for i < (face.len - 1) {
					v_seq << 0
					v_seq << i
					v_seq << (i + 1)					
					i++
				}
				//println("BIG FACES! ${fc} ${face.len} v_seq:${v_seq.len}")
			}
			
			// no vertex index, generate normals
			if face[0][1] == -1 && face.len >= 3 {
				mut v_count := 0
				v0 := face[v_count + 0][0]
				v1 := face[v_count + 1][0]
				v2 := face[v_count + 2][0]
				
				vec0 := obj_part.v[v2] - obj_part.v[v1]
				vec1 := obj_part.v[v0] - obj_part.v[v1]
				tmp_normal := vec0 % vec1
					
				for v_count < face.len {
					obj_part.vn << tmp_normal
					obj_part.part[in_part].faces[fc][v_count][1] = obj_part.vn.len - 1
					v_count ++
				}
			}
			
			for vertex_index in v_seq {
				// position
				if vertex_index >= face.len {
					continue
				}				
				v_index := face[vertex_index][0] // vertex index
				n_index := face[vertex_index][1] // normal index
				t_index := face[vertex_index][2] // uv texture index
				key := "${v_index}_${n_index}_${t_index}"
				if key !in cache {
					cache[key] = v_count_index
					mut pnct := Vertex_pnct {
						x: obj_part.v[v_index].e[0]
						y: obj_part.v[v_index].e[1]
						z: obj_part.v[v_index].e[2]
					}
					// normal
					if n_index >= 0 {
						pnct.nx = obj_part.vn[n_index].e[0]
						pnct.ny = obj_part.vn[n_index].e[1]
						pnct.nz = obj_part.vn[n_index].e[2]
					}
					// texture uv
					if t_index >= 0 {
						pnct.u = obj_part.vt[t_index].e[0]
						pnct.v = obj_part.vt[t_index].e[1]					
					}
					
					out_buf.vbuf << pnct
					out_buf.ibuf << u32(v_count_index)
					v_count_index++
				} else {
					//println("Cache used! $key")
					out_buf.ibuf << u32(cache[key])
					cache_hit++
				}
				
			}
			
		}
	}
	
	/*
	println("------------")
	for c1, x1 in out_buf.vbuf[..10] {
		println("$c1 $x1")
	}
	println(out_buf.ibuf[..10])
	*/
	//println("vbuf size: ${out_buf.vbuf.len} ibuf size: ${out_buf.ibuf.len} Cache hit: $cache_hit")
	out_buf.n_vertex = u32(out_buf.ibuf.len)
	return out_buf
}

//==============================================================================
// Utility
//==============================================================================
// print on the console the summary of the .obj model loaded
pub fn (obj_part ObjPart) summary() {
	println("---- Stats     ----")
	println("vertices: ${obj_part.v.len}")
	println("normals : ${obj_part.vn.len}")
	println("uv      : ${obj_part.vt.len}")
	println("parts   : ${obj_part.part.len}")
	// Parts
	println("---- Parts     ----")
	for c, x in obj_part.part {
		println("${c:3} [${x.name:-16}] mat:[${x.material:-10}] ${x.faces.len:7} faces")
	}
	// Materials
	println("---- Materials ----")
	println("Material dict: ${obj_part.mat_map.keys()}")
	for c, mat in obj_part.mat {
		println("${c:3} [${mat.name:-16}]")
		for k,v in mat.ks {
			print("$k = $v")
		}
		for k,v in mat.ns {
			println("$k = $v")
		}
		for k,v in mat.maps {
			println("$k = $v")
		}
	}
}

// debug test function, do not remove.
pub fn tst(){
/*
	//fname := "capsule.obj"
	//fname := "Forklift.obj"
	fname := "cube.obj"
	//fname := "Orange Robot 3D ObjPart.obj"
	
	mut obj := ObjPart{}
	buf := os.read_lines(fname) or { panic(err.msg) }
	obj.parse_obj_buffer(buf)
	obj.summary()
*/
/*
	a :="f 7048 7070 7071 7072 7073 7074 7075 7076 7077 7078 7079 7080 7003"
	mut f1 := 0
	mut f2 := 0
	f0,mut p := get_int(a,1)
	f1, p = get_int(a,p)
	f2, p = get_int(a,p)
	println("res: ${f0} ${f1} ${f2}")
*/
/*
	a :="v -0 0.107769 -0.755914"
	println("${parse_3f(a,1)}")
*/
/*
	ort := m4.ortho(0,300,0,200,0,0)
	println(ort)
	a := m4.vec3(0,0,0)
	println("a: $a")
	res := m4.mul_vec(ort, a)
	println("res:\n${res}")
*/
	s := "K 1 1 1"
	r := strconv.atof_quick(s[1..s.len-1])
	println(r)
}