" start vundle shit ===============================>
set nocompatible              " be iMproved, required for vundle
filetype off                  " required, required for vundle

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" the package manager itself
Plugin 'VundleVim/Vundle.vim' 

Plugin 'airblade/vim-gitgutter'

" git plugin - fugitive
Plugin 'tpope/vim-fugitive'

" airline plugin
Plugin 'vim-airline/vim-airline'
" themes for airline plugin
Plugin 'vim-airline/vim-airline-themes'
" tmuxline plugin
Plugin 'edkolev/tmuxline.vim'

" gruvbox theme
Plugin 'morhetz/gruvbox'

" tagbar plugin to visually show present tags
Plugin 'majutsushi/tagbar'

" surround
Plugin 'tpope/vim-surround'

" themes =====>
" afterglow theme
" Plugin 'danilo-augusto/vim-afterglow'
" alduin theme
" Plugin 'AlessandroYorba/Alduin'

call vundle#end()            " required
filetype plugin indent on    " required for vundle

set smartindent
set number
set relativenumber
set tabstop=2
set shiftwidth=2
set expandtab
syntax on

set guifont=Source\ Code\ Pro\ for\ Powerline\ Regular

set cursorline
set cursorcolumn

" fix delay on esc+shift+o
set timeout timeoutlen=5000 ttimeoutlen=100

" treat dash ( - ) as part of the word
set iskeyword+=-

" gruvbox theme config
" let g:gruvbox_contrast_dark = 'soft'
colorscheme gruvbox
set bg=dark

" Change line color when entering Insert Mode
highlight CursorLine ctermbg=235 ctermfg=None
autocmd InsertLeave * highlight CursorLine ctermbg=235 ctermfg=None
autocmd InsertEnter * highlight CursorLine ctermbg=233 ctermfg=None

" different colors for matching brackets
" hi MatchParen cterm=none ctermbg=green ctermfg=blue

" to enable recursive file finding
set path+=**

" line to tell me when i've reached max characters count int line
highlight ColorColumn ctermbg=darkred
set colorcolumn=72

" java specific macros ==================>
let @g="mn^wyt GOpublic pa get`nyiw$Gk$apblllvUA()€ü {}Oreturn pa;"

" for airline
set encoding=utf-8
let g:airline_powerline_fonts=1
let g:Powerline_symbols='unicode'
let g:airline_theme='wombat'

" for gitgutter
let g:gitgutter_map_keys = 0 " no need for map keys.. yet i guess?

" enable italic font
let g:sublimemonokai_term_italic = 1

nmap <F8> :TagbarToggle<CR>

" open tagbar by default when vim opens
autocmd FileType * call tagbar#autoopen(0)
