module fastc

import os
import v.cmdexec

fn test_reflected_integer_variants_reparse_canonical_source_types() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_variant_source_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

type Number = uint | string
type Containers = []uint | map[string]uint | string
type Counter = uint
type AliasedNumber = Counter | string

fn check_number(value Number) {
 mut matches := 0
 mut misses := 0
 mut zeros := 0
 $for variant in Number.variants {
  if value is variant { matches++ }
  if value !is variant { misses++ }
  zero := Number(variant)
  if zero is variant { zeros++ }
  typed_zero := Number($zero(variant.typ))
  if typed_zero is variant { zeros++ }
  assert variant.typ > 0
 }
 assert matches == 1
 assert misses == 1
 assert zeros == 4
}

fn check_container(value Containers) {
 mut matches := 0
 $for variant in Containers.variants {
  if value is variant { matches++ }
 }
 assert matches == 1
}

fn main() {
 check_number(Number(uint(7)))
 check_number(Number("value"))
 check_container(Containers([uint(1)]))
 check_container(Containers({"value": uint(2)}))
 check_container(Containers("value"))
 $for variant in AliasedNumber.variants {
  $if variant.typ is Counter {
   zero := $zero(variant.typ.unaliased_typ)
   assert zero == uint(0)
  }
 }
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_integer_option_names_narrow_in_if_expressions() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_option_expression_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

fn signed(value ?int) int {
 isize := value
 result := if isize != none { isize } else { -1 }
 if isize != none { assert result == isize }
 return result
}

fn unsigned(value ?uint) uint {
 usize := value
 result := if usize != none { usize } else { uint(9) }
 if usize != none { assert result == usize }
 return result
}

fn main() {
 assert signed(?int(3)) == 3
 assert signed(?int(none)) == -1
 assert unsigned(?uint(uint(5))) == uint(5)
 assert unsigned(?uint(none)) == uint(9)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
