" Vim filetype detection for the V language.

augroup v_filetypedetect
	autocmd!
	autocmd BufRead,BufNewFile *.v,*.vsh,*.vv setfiletype v
augroup END
