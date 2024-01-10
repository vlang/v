import os
import v.pkgconfig

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

const samples_dir = os.join_path(vroot, 'vlib', 'v', 'pkgconfig', 'test_samples')

fn test_vexe_and_vroot_exist() {
	assert vexe != ''
	assert vroot != ''
	assert os.is_file(vexe)
	assert os.is_dir(vroot)
	assert os.is_dir(samples_dir)
}

fn test_dependency_resolution_fails_correctly() {
	pc_files := os.walk_ext(samples_dir, '.pc')
	assert pc_files.len > 0
	mut errors := []string{}
	for pc in pc_files {
		pcname := os.file_name(pc).replace('.pc', '')
		pkgconfig.load(pcname, use_default_paths: false, path: samples_dir) or {
			errors << err.msg()
		}
	}
	assert errors.len < pc_files.len
	assert errors == ['could not resolve dependency xyz-unknown-package']
}

fn test_samples() {
	pc_files := os.walk_ext(samples_dir, '.pc')
	assert pc_files.len > 0
	for pc in pc_files {
		pcname := os.file_name(pc).replace('.pc', '')
		x := pkgconfig.load(pcname, use_default_paths: false, path: samples_dir) or {
			if pcname == 'dep-resolution-fail' {
				continue
			}
			println('>>> err: ${err}')
			assert false
			return
		}
		assert x.name != ''
		assert x.modname != ''
		assert x.version != ''
		if pcname == 'gmodule-2.0' {
			assert x.name == 'GModule'
			assert x.modname == 'gmodule-2.0'
			assert x.url == ''
			assert x.version == '2.64.3'
			assert x.description == 'Dynamic module loader for GLib'
			assert x.libs == ['-Wl,--export-dynamic', '-L/usr/lib/x86_64-linux-gnu', '-lgmodule-2.0',
				'-pthread', '-lglib-2.0', '-lpcre']
			assert x.libs_private == ['-ldl', '-pthread']
			assert x.cflags == ['-I/usr/include', '-pthread', '-I/usr/include/glib-2.0',
				'-I/usr/lib/x86_64-linux-gnu/glib-2.0/include']
			assert x.vars == {
				'prefix':            '/usr'
				'libdir':            '/usr/lib/x86_64-linux-gnu'
				'includedir':        '/usr/include'
				'gmodule_supported': 'true'
			}
			assert x.requires == ['gmodule-no-export-2.0', 'glib-2.0']
			assert x.requires_private == []
			assert x.conflicts == []
		}
		if x.name == 'expat' {
			assert x.url == 'http://www.libexpat.org'
		}
		if x.name == 'GLib' {
			assert x.modname == 'glib-2.0'
			assert x.libs == ['-L/usr/lib/x86_64-linux-gnu', '-lglib-2.0', '-lpcre']
			assert x.libs_private == ['-pthread']
			assert x.cflags == ['-I/usr/include/glib-2.0',
				'-I/usr/lib/x86_64-linux-gnu/glib-2.0/include', '-I/usr/include']
			assert x.vars == {
				'prefix':          '/usr'
				'libdir':          '/usr/lib/x86_64-linux-gnu'
				'includedir':      '/usr/include'
				'bindir':          '/usr/bin'
				'glib_genmarshal': '/usr/bin/glib-genmarshal'
				'gobject_query':   '/usr/bin/gobject-query'
				'glib_mkenums':    '/usr/bin/glib-mkenums'
			}
			assert x.requires_private == ['libpcre']
			assert x.version == '2.64.3'
			assert x.conflicts == []
		}
	}
}
