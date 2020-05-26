import os
import v.pref

fn main(){
	vexe := pref.vexe_path()
	$if windows {
		vdir := os.real_path(os.dir(vexe))
		res := os.system('setx /M PATH "$vdir;%PATH%"')
		if res == 0 {
			println('v has been prepended to the path')
			exit(0)
		}
		exit(1)
	}
	//
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
