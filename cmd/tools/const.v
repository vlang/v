// consts for v compiler and v tools
pub const (
	v_repos   = [
		'https://github.com/vlang/v',	//master repo
		'https://hub.fastgit.org/vlang/v',
		'https://gitee.com/mirros/vlang',	//Chinese mirror
	]
	vc_repos  = [
		'https://hub.fastgit.org/vlang/vc',
		'https://hub.fastgit.org/vlang/vc'
	]
	tcc_win   = 'tcc -Bthirdparty/tcc -Ithirdparty/stdatomic/win -bt10 -g -w -o v.exe vc\v_win.c -ladvapi32'
	gcc_win   = 'gcc -std=c99 -municode -Ithirdparty/stdatomic/win -g -w -o v.exe .\vc\v_win.c -ladvapi32'
	clang_win = 'clang -std=c99 -Ithirdparty/stdatomic/win -municode -g -w -o v.exe .\vc\v_win.c -ladvapi32'
	tcc_nix   = 'tcc -std=gnu11 -w -I ./thirdparty/stdatomic/nix -o ./v vc/v.c -lm -lexecinfo -lpthread'
	gcc_nix   = 'gcc -std=gnu11 -w -I ./thirdparty/stdatomic/nix -o ./v vc/v.c -lm -lexecinfo -lpthread'
	clang_nix = 'c -std=gnu11 -w -I ./thirdparty/stdatomic/nix -o ./v vc/v.c -lm -lexecinfo -lpthread'
)
