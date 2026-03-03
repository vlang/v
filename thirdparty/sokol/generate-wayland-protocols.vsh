#!/usr/bin/env v run

// Wayland Protocol Generation Script
// Generates C headers and implementation files for Wayland protocols
import os
import log

const protocols_dir = '/usr/share/wayland-protocols'
const output_dir = '.'

struct Protocol {
	xml_path  string
	base_name string
}

fn main() {
	log.info('==========================================')
	log.info('Generating Wayland Protocol Files')
	log.info('==========================================')

	// Check if wayland-scanner is available
	if !can_run_command('wayland-scanner --version') {
		eprintln('Error: wayland-scanner not found in PATH')
		eprintln('Please install wayland-protocols and wayland-scanner:')
		eprintln('  Debian/Ubuntu: sudo apt install wayland-protocols wayland-scanner')
		eprintln('  Fedora: sudo dnf install wayland-protocols-devel wayland-scanner')
		eprintln('  Arch: sudo pacman -S wayland-protocols wayland')
		exit(1)
	}

	// Define all protocols to generate
	protocols := [
		Protocol{
			xml_path:  '${protocols_dir}/stable/xdg-shell/xdg-shell.xml'
			base_name: 'xdg-shell'
		},
		Protocol{
			xml_path:  '${protocols_dir}/staging/fractional-scale/fractional-scale-v1.xml'
			base_name: 'fractional-scale-v1'
		},
		Protocol{
			xml_path:  '${protocols_dir}/staging/cursor-shape/cursor-shape-v1.xml'
			base_name: 'cursor-shape-v1'
		},
		Protocol{
			xml_path:  '${protocols_dir}/staging/xdg-toplevel-icon/xdg-toplevel-icon-v1.xml'
			base_name: 'xdg-toplevel-icon-v1'
		},
		Protocol{
			xml_path:  '${protocols_dir}/unstable/pointer-constraints/pointer-constraints-unstable-v1.xml'
			base_name: 'pointer-constraints-unstable-v1'
		},
		Protocol{
			xml_path:  '${protocols_dir}/unstable/relative-pointer/relative-pointer-unstable-v1.xml'
			base_name: 'relative-pointer-unstable-v1'
		},
		Protocol{
			xml_path:  '${protocols_dir}/unstable/xdg-decoration/xdg-decoration-unstable-v1.xml'
			base_name: 'xdg-decoration-unstable-v1'
		},
		Protocol{
			xml_path:  '${protocols_dir}/stable/viewporter/viewporter.xml'
			base_name: 'viewporter'
		},
		Protocol{
			xml_path:  '${protocols_dir}/unstable/tablet/tablet-unstable-v2.xml'
			base_name: 'tablet-unstable-v2'
		},
	]

	// Generate protocol files
	for protocol in protocols {
		if os.exists(protocol.xml_path) {
			generate_protocol(protocol.xml_path, protocol.base_name)
		} else {
			log.info('Skipping ${protocol.base_name} (XML not found: ${protocol.xml_path})')
		}
	}

	// Compile protocol .c files to .o and create static libraries
	log.info('')
	log.info('Compiling protocol object files and creating static libraries...')
	mut compiled_count := 0
	for protocol in protocols {
		c_file := '${protocol.base_name}-protocol.c'
		if os.exists(c_file) {
			base_name := protocol.base_name
			o_file := '${base_name}-protocol.o'
			a_file := 'lib${base_name}-protocol.a'

			log.info('  Compiling ${c_file} -> ${o_file}')
			run_command('gcc -c -fPIC -fvisibility=default ${c_file} -o ${o_file}')

			log.info('  Creating static library ${a_file}')
			run_command('ar rcs ${a_file} ${o_file}')

			compiled_count++
		}
	}

	log.info('')
	log.info('==========================================')
	log.info('Protocol Generation Complete!')
	log.info('==========================================')
	log.info('')
	log.info('Generated ${compiled_count} protocol libraries')
	log.info('')
	log.info('To use these protocols in your build:')
	log.info('  1. Include the *-client-protocol.h headers in your C code')
	log.info('  2. Link the lib*-protocol.a static libraries')
	log.info('  3. Link with -lwayland-client')
}

fn generate_protocol(xml_file string, base_name string) {
	log.info('Generating ${base_name}...')

	// Generate client header
	header_file := '${output_dir}/${base_name}-client-protocol.h'
	run_command('wayland-scanner client-header ${xml_file} ${header_file}')
	log.info('  ✓ ${base_name}-client-protocol.h')

	// Generate protocol code
	code_file := '${output_dir}/${base_name}-protocol.c'
	run_command('wayland-scanner private-code ${xml_file} ${code_file}')
	log.info('  ✓ ${base_name}-protocol.c')
}

fn run_command(cmd string) {
	result := os.system(cmd)
	if result != 0 {
		eprintln('Command failed: ${cmd}')
		eprintln('Exit code: ${result}')
		exit(1)
	}
}

fn can_run_command(cmd string) bool {
	result := os.system(cmd)
	return result == 0
}
