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
Plugin 'AlessandroYorba/Alduin'
Plugin 'liuchengxu/space-vim-dark'
Plugin 'morhetz/gruvbox'
Plugin 'tpope/vim-surround'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/nerdtree'

call vundle#end()
filetype plugin indent on

set smartindent
set number
set relativenumber
set tabstop=2
set shiftwidth=2
set expandtab
syntax on

let g:gruvbox_contrast_dark='hard'
" colorscheme gruvbox
" set bg=dark
color zellner

set guifont=Source\ Code\ Pro\ for\ Powerline\ Regular

" fix delay on esc+shift+o
set timeout timeoutlen=5000 ttimeoutlen=100

set cursorline
set cursorcolumn
highlight CursorLine ctermfg=None cterm=bold term=bold
" autocmd InsertLeave * highlight CursorLine ctermbg=53 ctermfg=None cterm=bold term=bold
autocmd InsertLeave * highlight CursorLine ctermfg=None cterm=bold term=bold
autocmd InsertEnter * highlight CursorLine ctermfg=None cterm=italic term=italic
highlight CursorColumn ctermfg=None cterm=bold term=bold ctermbg=none
autocmd InsertEnter * highlight CursorColumn ctermfg=None cterm=italic term=italic
autocmd InsertLeave * highlight CursorColumn ctermfg=None cterm=bold term=bold

" change visual mode highlight color
hi Visual term=reverse ctermbg=234 guibg=Grey
hi Search term=reverse ctermbg=234 guibg=Grey

" make comments in italic font
hi comment term=italic cterm=italic

" to enable recursive file finding
set path+=**

" for airline
set encoding=utf-8
let g:airline_powerline_fonts=1
let g:Powerline_symbols='unicode'
let g:airline_theme='angr'

nnoremap Y y$

" C-n to start nerdtree
map <C-n> :NERDTreeToggle<CR>
" relative numbers for nerdtree
" enable line numbers
let NERDTreeShowLineNumbers=1
" make sure relative line numbers are used
autocmd FileType nerdtree setlocal relativenumber
" use tab instead of enter, its easier to reach
autocmd FileType nerdtree nmap <buffer> <Tab> <Enter>
" start nerdtree when vim starts
" autocmd vimenter * NERDTree
" exit vim if the only window left open is nerdtree's
autocmd BufEnter * nested if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
