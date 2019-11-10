// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
  os
)

struct Create {
mut:
  name string
  description string
}

fn (c Create)write_vmod() {
  vmod := os.create('${c.name}/v.mod') or {
    println('')
    eprintln('error: $err')
    exit(1)
  }
  mut vmod_content := []string
  vmod_content << '#V Project#\n'
  vmod_content << 'Module {'
  vmod_content << '  name: \'${c.name}\','
  vmod_content << '  description: \'${c.description}\','
  vmod_content << '  dependencies: []'
  vmod_content << '}'
  vmod.write(vmod_content.join('\n'))
}

fn (c Create)write_main() {
  main := os.create('${c.name}/${c.name}.v') or {
    println('')
    eprintln('error: $err')
    exit(1)
  }
  mut main_content := []string
  main_content << 'module main\n'
  main_content << 'fn main() {'
  main_content << '  println(\'Hello World !\')'
  main_content << '}'
  main.write(main_content.join('\n'))

}

fn main() {
  mut c := Create{}
  print('Choose your project name: ')
  c.name = os.get_line()
  print('Choose your project description: ')
  c.description = os.get_line()
  println('Initialising ...')
  if (os.is_dir(c.name)) {
    println('')
    eprintln('error: folder already exists')
    exit(1)
  }
  os.mkdir(c.name)
  c.write_vmod()
  c.write_main()
  println('Complete !')
  exit (0)
}