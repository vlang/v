import dl
import os

type Func = fn () int

fn run_test(cc_used string) {
	cc_flag := match cc_used {
		'tcc64' { '-cc tcc' }
		'gcc64' { '-cc gcc' }
		'clang64' { '-cc clang' }
		'msvc64' { '-cc msvc' }
		'tcc32' { '-cc tcc -m32 -d no_backtrace' }
		'gcc32' { '-cc gcc -m32' }
		'clang32' { '-cc clang -m32' }
		'msvc32' { '-cc msvc -m32' }
		else { '' }
	}

	assert os.system('${os.quoted_path(@VEXE)} ${cc_flag} -o create_win_${cc_used}.dll -shared create_win_dll.v') == 0
	assert os.exists('create_win_${cc_used}.dll')
	handle := dl.open('create_win_${cc_used}.dll', 0)
	assert handle != 0
	test := Func(dl.sym(handle, 'Tatltuae'))
	assert test() == 42
	assert test() != 666
	// dl.close(handle)  // works for gcc, clang and msvc but crashes with tcc
}

fn test_create_and_dllmain() {
	os.chdir(os.dir(@FILE)) or {}
	$if windows {
		$if x64 {
			$if tinyc {
				run_test('tcc64')
			}
			$if gcc {
				run_test('gcc64')
			}
			$if clang {
				run_test('clang64')
			}
			$if msvc {
				run_test('msvc64')
			}
		}
		$if x32 {
			$if tinyc {
				run_test('tcc32')
			}
			$if gcc {
				run_test('gcc32')
			}
			$if clang {
				run_test('clang32')
			}
			$if msvc {
				// run_test('msvc32')  // something wrong as it passes when it should fail
			}
		}
	}
}
