# V Vim/Neovim runtime files

This directory contains official V runtime files for Vim and Neovim:

- `ftdetect/v.vim`
- `ftplugin/v.vim`
- `syntax/v.vim`

## Install in Vim

```sh
mkdir -p ~/.vim/pack/vlang/start
ln -s /path/to/v/editors/vim ~/.vim/pack/vlang/start/vlang
```

## Install in Neovim

```sh
mkdir -p ~/.local/share/nvim/site/pack/vlang/start
ln -s /path/to/v/editors/vim ~/.local/share/nvim/site/pack/vlang/start/vlang
```

Restart Vim/Neovim after installation.
