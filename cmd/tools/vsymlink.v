import os
import v.pref

fn main(){
	$if windows {
		setup_symlink_on_windows()
	} $else {
		setup_symlink_on_unix()
	}
}

fn setup_symlink_on_unix(){
	vexe := pref.vexe_path()
	mut link_path := '/usr/local/bin/v'
	mut ret := os.exec('ln -sf $vexe $link_path') or {
		panic(err)
	}
	if ret.exit_code == 0 {
		println('Symlink "$link_path" has been created')
	} else if os.system("uname -o | grep -q \'[A/a]ndroid\'") == 0 {
		println('Failed to create symlink "$link_path". Trying again with Termux path for Android.')
		link_path = '/data/data/com.termux/files/usr/bin/v'
		ret = os.exec('ln -sf $vexe $link_path') or {
			panic(err)
		}
		if ret.exit_code == 0 {
			println('Symlink "$link_path" has been created')
		} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
		}
	} else {
		println('Failed to create symlink "$link_path". Try again with sudo.')
	}
}

fn setup_symlink_on_windows(){
	vexe := pref.vexe_path()
	// NB: Putting $vdir directly into PATH will also result in
	// make.bat being global, which is NOT what we want.
	//
	// Instead, we create a small launcher v.bat, in a new local
	// folder .symlink/ . That .symlink/ folder can then be put
	// in PATH without poluting it with anything else - just a
	// `v` command will be available, simillar to unix.
	//
	// Creating a real NTFS symlink to the real executable was also
	// tried, but then os.real_path( os.executable() ) returns the
	// path to the symlink, unfortunately, unlike on posix systems
	// ¯\_(ツ)_/¯
	vdir := os.real_path(os.dir(vexe))
	vsymlinkdir := os.join_path(vdir, '.symlink')
	vsymlinkbat := os.join_path(vsymlinkdir, 'v.bat')
	os.rmdir_all(vsymlinkdir)
	os.mkdir_all(vsymlinkdir)
	os.write_file(vsymlinkbat, '@echo off\n${vexe} %*')
	if !os.exists( vsymlinkbat ) {
		eprintln('Could not create $vsymlinkbat')
		exit(1)
	}
	println('Created $vsymlinkbat .')
	current_paths := os.getenv('PATH').split(';').map(it.trim('/\\'))
	if vsymlinkdir in current_paths {
		println('$vsymlinkdir is already on your PATH')
		println('Try running `v version`')
		exit(0)
	}
	// put vsymlinkdir first, prevent duplicates:
	mut new_paths := [ vsymlinkdir ]
	for p in current_paths {
		if p !in new_paths {
			new_paths << p
		}
	}
	//
	change_path_cmd := 'setx /M PATH "' + new_paths.join(';') +'"'
	println('Changing global PATH with:')
	println(change_path_cmd)
	res := os.system(change_path_cmd)
	if res == 0 {
		println('')
		println('$vsymlinkdir has been prepended to PATH.')
		println('Try running `v version`.')
		exit(0)
	} else {
		println('Could not run `setx`, probably you are not an administrator.')
		println('`v symlink` should be launched with admin privileges.')
		exit(1)
	}
}
