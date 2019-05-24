" start vundle shit ===============================>
set nocompatible              " be iMproved, required for vundle
filetype off                  " required, required for vundle

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

Plugin 'VundleVim/Vundle.vim' 
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'edkolev/tmuxline.vim'
Plugin 'AlessandroYorba/Alduin'
Plugin 'liuchengxu/space-vim-dark'
Plugin 'morhetz/gruvbox'
Plugin 'tpope/vim-surround'
Plugin 'anned20/vimsence'

call vundle#end()            " required
filetype plugin indent on    " required for vundle

set smartindent
set number
set relativenumber
set tabstop=2
set shiftwidth=2
set expandtab
syntax on

let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox
set bg=dark

set guifont=Source\ Code\ Pro\ for\ Powerline\ Regular

" fix delay on esc+shift+o
set timeout timeoutlen=5000 ttimeoutlen=100

set cursorline
set cursorcolumn
highlight CursorLine ctermbg=235 ctermfg=None
autocmd InsertLeave * highlight CursorLine ctermbg=235 ctermfg=None
autocmd InsertEnter * highlight CursorLine ctermbg=233 ctermfg=None
highlight CursorColumn ctermbg=235 ctermfg=None
autocmd InsertEnter * highlight CursorColumn ctermbg=233 ctermfg=None
autocmd InsertLeave * highlight CursorColumn ctermbg=235 ctermfg=None

" to enable recursive file finding
set path+=**

" for airline
set encoding=utf-8
let g:airline_powerline_fonts=1
let g:Powerline_symbols='unicode'
let g:airline_theme='wombat'

nnoremap Y y$
