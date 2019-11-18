set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

Plugin 'VundleVim/Vundle.vim' 
Plugin 'morhetz/gruvbox'
Plugin 'scrooloose/nerdtree'
" Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-surround'
Plugin 'AlessandroYorba/Alduin'
Plugin 'tpope/vim-fugitive'
Plugin 'wincent/command-t'
Plugin 'scrooloose/nerdcommenter'
Plugin 'junegunn/fzf'
Plugin 'dylanaraps/wal'

call vundle#end()
filetype plugin indent on

set smartindent
set number
set relativenumber
set tabstop=4
set shiftwidth=4
set expandtab
syntax on
" set smartcase " search will be case sensitive if it contains uppercase
set ignorecase
set incsearch
set mouse=a

" let g:gruvbox_contrast_dark='hard'
" colorscheme alduin
" set bg=dark
" set cursorline
" set cursorcolumn
" highlight CursorLine ctermfg=None cterm=bold term=bold ctermbg=233
" autocmd InsertLeave * highlight CursorLine ctermbg=53 ctermfg=None cterm=bold term=bold
" autocmd InsertLeave * highlight CursorLine ctermfg=None cterm=bold term=bold ctermbg=235
" autocmd InsertEnter * highlight CursorLine ctermfg=None ctermbg=233
" highlight CursorColumn ctermfg=None cterm=bold term=bold ctermbg=235
" autocmd InsertEnter * highlight CursorColumn ctermfg=None ctermbg=233
" autocmd InsertLeave * highlight CursorColumn ctermfg=None cterm=bold term=bold ctermbg=235

set guifont=Inconsolata
set guioptions=""

" fix delay on esc+shift+o
set timeout timeoutlen=5000 ttimeoutlen=100

" change visual mode highlight color
" hi Visual term=reverse ctermbg=234 guibg=Grey
" hi Search term=reverse ctermbg=234 guibg=Grey

" make comments in italic font
" hi comment term=italic cterm=italic

" to enable recursive file finding
set path+=**
" set swap files directory
set directory=$HOME/.vim//

" for airline
set encoding=utf-8
" let g:airline_powerline_fonts=1
" let g:Powerline_symbols='unicode'

nnoremap Y y$
nnoremap <C-h> :cprev<Return>
nnoremap <C-l> :cnext<Return>
inoremap <C-e> <Esc><C-e>a
inoremap <C-y> <Esc><C-y>a
nnoremap <C-x>c :e! ~/.vimrc<CR>
nnoremap <C-x>r :so ~/.vimrc<CR>
" exit buffer without closing window
nnoremap <C-x>k :bp\|bd #<CR>
nnoremap <leader>f :FZF<CR>

" C-n to start nerdtree
map <C-n> :NERDTreeToggle<CR>
" relative numbers for nerdtree
" enable line numbers
let NERDTreeShowLineNumbers=1
" make sure relative line numbers are used
autocmd FileType nerdtree setlocal relativenumber
" nerd tree key mapping
autocmd FileType nerdtree nmap <buffer> <Tab> <Enter>
autocmd FileType nerdtree nmap <buffer> dd mdy

" start nerdtree when vim starts
" autocmd vimenter * NERDTree
" exit vim if the only window left open is nerdtree's
autocmd BufEnter * nested if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

set background=dark
set t_Co=256

" tagbar
nmap <C-s> :TagbarToggle<CR>
" make sure relative line numbers are used
autocmd FileType tagbar setlocal relativenumber
" tagbar tree key mapping
autocmd FileType tagbar nmap <buffer> <Tab> <Enter>
autocmd FileType tagbar nmap <buffer> dd mdy

" java specific macros
" print macro
let @p = 'ISystem.out.println(A);==:w'

" highlight CursorLine ctermfg=None cterm=bold term=bold ctermbg=235

" use system clipboard register by default, copy and paste from it by default
set clipboard=unnamedplus

autocmd VimEnter * silent exec "! echo -ne '\e[1 q'"
autocmd VimLeave * silent exec "! echo -ne '\e[5 q'" 
let &t_SI = "\<esc>[5 q"
let &t_SR = "\<esc>[4 q"
let &t_EI = "\<esc>[2 q"

command! Compile !./compile
command! Run !./compile && ./run

" nerdtree cd
let g:NERDTreeChDirMode = 2

color wal
