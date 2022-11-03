set nocompatible
filetype off

" set the runtime path to include Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim' 
Plugin 'tpope/vim-surround'
"Plugin 'AlessandroYorba/Alduin'
Plugin 'scrooloose/nerdcommenter'
Plugin 'junegunn/fzf'
Plugin 'morhetz/gruvbox'
call vundle#end()
filetype plugin indent on

set smartindent
set number
set relativenumber
set tabstop=2
set shiftwidth=2
set expandtab
syntax on
set smartcase " search will be case sensitive if it contains uppercase
set ignorecase
set incsearch
set mouse=a
set autoread
au FocusGained,BufEnter * :checktime
:set nofixendofline " stop inserting newline at end of file

" fix delay on esc+shift+o
set timeout timeoutlen=5000 ttimeoutlen=100

" to enable recursive file finding
set path+=**
" set swap files directory
set directory=$HOME/.vim//

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
" this doesnt work <C-c> tries to exit
nnoremap <C-w><C-c> <C-w>c
nnoremap diP vipjd
nnoremap ci$ T$ct$
nnoremap cw ciw
nnoremap dw diw

" set t_Co=256

" use system clipboard register by default, copy and paste from it by default
set clipboard=unnamedplus

autocmd VimEnter * silent exec "! echo -ne '\e[1 q'"
autocmd VimLeave * silent exec "! echo -ne '\e[5 q'" 
let &t_SI = "\<esc>[5 q"
let &t_SR = "\<esc>[4 q"
let &t_EI = "\<esc>[2 q"

function! TurnOffColors()
    :set t_Co=0
    syntax off
    :highlight LineNr NONE
    :highlight CursorLine NONE
    " Add any other necessary highlight lines here
endfunction

command! TurnOffColors call TurnOffColors()
" TurnOffColors

" disable welcome message
set shortmess+=I

"color gruvbox
set bg=dark

" search without highlighting
:set nohlsearch
