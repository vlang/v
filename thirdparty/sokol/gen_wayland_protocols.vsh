#!/usr/bin/env -S v

import log

fn sh(cmd string) {
	log.info('Executing: ❯ ${cmd}')
	print(execute_or_exit(cmd).output)
}

fn gen_protocol(protocols_dir string, output_dir string, name string, path string) {
	log.info('Generating protocol: ${name}')
	sh('wayland-scanner client-header ${protocols_dir}/${path} ${output_dir}/${name}-client-protocol.h')
	sh('wayland-scanner private-code ${protocols_dir}/${path} ${output_dir}/${name}-protocol.c')
}

protocols_dir := '/usr/share/wayland-protocols'
output_dir := '.'
protocols := {
	'xdg-shell':                       '/stable/xdg-shell/xdg-shell.xml'
	'viewporter':                      '/stable/viewporter/viewporter.xml'
	'fractional-scale-v1':             '/staging/fractional-scale/fractional-scale-v1.xml'
	'cursor-shape-v1':                 '/staging/cursor-shape/cursor-shape-v1.xml'
	'xdg-toplevel-icon-v1':            '/staging/xdg-toplevel-icon/xdg-toplevel-icon-v1.xml'
	'tablet-unstable-v2':              '/unstable/tablet/tablet-unstable-v2.xml'
	'pointer-constraints-unstable-v1': '/unstable/pointer-constraints/pointer-constraints-unstable-v1.xml'
	'relative-pointer-unstable-v1':    '/unstable/relative-pointer/relative-pointer-unstable-v1.xml'
	'xdg-decoration-unstable-v1':      '/unstable/xdg-decoration/xdg-decoration-unstable-v1.xml'
}

for protocol, path in protocols {
	gen_protocol(protocols_dir, output_dir, protocol, path)
}
