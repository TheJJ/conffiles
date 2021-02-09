"
" JJ's vimrc
"
" released under the GPLv3 or later
" (c) 2008 - 2021 Jonas Jelten


syntax on
set ai                             "autoindent
set nonu                           "no line numbers
set nocp                           "vim, not vi
set shiftround                     "round > < to shiftwidth
set ignorecase smartcase           "ignore case, except if contains uppercase
set hlsearch                       "highlight search results
set ff=unix                        "unix line endings
set noerrorbells visualbell t_vb=  "don't ring the fucking bell
set termencoding=utf-8             "utf-8 terminal
set encoding=utf-8                 "utf-8 encoding
set showmatch                      "matching parentheses
set incsearch                      "incremental search
set mouse=a                        "enable mouse
set nocursorline                   "underline current cursor position with line
set wildmenu                       "visual autocomplete for command menu
set lazyredraw                     "redraw only when we need to.
set hidden                         "hide modified buffers and don't warn
set nowrapscan                     "don't wrap the search
set nostartofline                  "don't move to first non-space char in line
set endofline                      "make sure last line ends with \n
set backspace=indent,eol,start     "expected backspace behavior
set ttyfast                        "of course we're in a fast tty
set ruler                          "enable the bottom line
set textwidth=0                    "no auto wrapping
set wrapmargin=0                   "really no auto wrapping
set scrolloff=4                    "lines to keep visible when scrolling
set formatoptions-=cro             "no 'helpful' comment continuation
set title                          "set x11 window title

filetype plugin on                 "per-filetype settings
filetype indent on                 "per-filetype indentation

set wildignorecase
set wildmode=longest:full,full
set wildignore+=*~
set wildignore+=*.swp

if has("gui_running")
	set mouse=a
	set nomh              " don't hide the mouse
	set guioptions+=imgT  " icon, menu bar, grey invalid items, show toolbar
	set linespace=0       " no extra pixels between lines
	set mouses=i-r:beam,s:updown,sd:udsizing,vs:leftright,vd:lrsizing,m:no,ml:up-arrow,v:rightup-arrow
else
	set t_Co=256
endif

autocmd GUIEnter * set visualbell t_vb=
autocmd FileType text setlocal textwidth=0
autocmd BufRead * let &l:modifiable = !&readonly   " prevent editing readonly files

scriptencoding utf-8

" whitespace highlighting: eol:¬,
set list listchars=tab:╺╴,trail:·,extends:→,precedes:←

set background=dark
" not totally crappy themes: elflord, ron, desert, murphy, slate, koehler
colorscheme ron
highlight SpecialKey ctermfg=darkblue
highlight SpecialKey guifg=#808080

if has('nvim')
	" neovim SHAred DAta
	" https://neovim.io/doc/user/options.html#'shada'
	"  !    : global vars
	"  '300 : remember marks of previously edited files
	"  <100 : lines for each register
	"  :100 : command line history
	"  s    : max size in kb
	"  %    : save and restore buffer list
	"  h    : no hlsearch on startup
	set shada=!,'300,<100,:1000,s100,%
else
	" Tell vim to remember certain things when we exit
	"  '10  :  marks will be remembered for up to 10 previously edited files
	"  "100 :  will save up to 100 lines for each register
	"  :... :  command-line history will be remembered
	"  %    :  saves and restores the buffer list
	"  n... :  where to save the viminfo files
	set viminfo='200,\"100,:1000,%,n~/.viminfo
endif

function! RestoreCursorPosition()
	if line("'\"") <= line("$")
		normal! g`"
		return 1
	endif
endfunction

augroup resCur
	autocmd!
	autocmd BufWinEnter * call RestoreCursorPosition()
augroup END

" use w!! to save files with sudo
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" :diffw for current diff
cmap diffw exec 'w !git diff -R --no-index -- - ' . shellescape(expand('%'))

" diff refresh on write
autocmd BufWritePost * if &diff == 1 | diffupdate | endif

" different colorscheme for diff
" themes that are not totally crappy: slate, murphy, industry
if &diff
	colorscheme murphy
endif

" refresh vimrc (this file) after saving
autocmd BufWritePost ~/.vimrc source %

" autosave delay, cursorhold trigger, default: 4000ms
setl updatetime=300

" highlight the word under cursor (CursorMoved is inperformant)
highlight WordUnderCursor cterm=underline gui=underline
autocmd CursorHold * call HighlightCursorWord()
function! HighlightCursorWord()
	" jj-specialhack: if hlsearch is active, don't overwrite it!
	let search = getreg('/')
	let cword = expand('<cword>')
	if match(cword, search) == -1
		exe printf('match WordUnderCursor /\V\<%s\>/', escape(cword, '/\'))
	endif
endfunction


" new navigation keys
inoremap <M-j> <Esc>hi
inoremap <M-k> <Esc>ki
inoremap <M-l> <Esc>ji
inoremap <M-;> <Esc>li
cnoremap <M-j> h
cnoremap <M-k> k
cnoremap <M-l> j
cnoremap <M-;> l

inoremap <C-Tab> <Tab>

" x clipboard
vmap <M-w> "+y
vmap <C-x> "+c
vmap <C-y> c<ESC>"+p
imap <C-y> "+pA

" control-backspace and control-delete for backward and forward word removal
imap <C-BS> <C-w>
noremap! <C-BS> <C-w>

imap <C-Del> <C-O>de
" ctrl-backspace on kitty (entered with insert-mode C-v)
imap [3;5~ <C-o>dw

noremap! <C-h> <C-w>
noremap! <C-Del> <C-O>de

" more readline-like word deletion
inoremap <C-w> <C-\><C-o>dB
inoremap <C-BS> <C-\><C-o>db


" no deselect when shifting
vnoremap < <gv
vnoremap > >gv

" font face for bad whitespace
highlight evilws ctermbg=red


" whitespace configuration
function! WhitespaceIndentWidth(x)
	let &l:tabstop     = a:x
	let &l:shiftwidth  = a:x
	let &l:softtabstop = a:x
endfunction

"autocmd ColorScheme * highlight Tab ctermbg=darkgreen guibg=darkgreen
"let ws=matchadd('evilws', '/[^\t]\zs\t\+\|\s\+\%#\@<!$/')
"autocmd InsertLeave * redraw!

" indent with spaces:
function! WhitespaceSpace(x)
	setlocal expandtab
	call WhitespaceIndentWidth(a:x)

	" tab indent /^\t+\zs/
	" trailing ws /\s\+\%#\@<!$/
	:2match evilws /^\t+\zs\|\s\+\%#\@<!$/
endfunction

" indent with tabs:
function! WhitespaceTab(x)
	setlocal noexpandtab
	call WhitespaceIndentWidth(a:x)

	" space indent /^\t*\zs \+/
	" stray tab /[^\t]\zs\t\+/
	" trailing ws /\s\+\%#\@<!$/
	:2match evilws /^\t*\zs \+\|[^\t]\zs\t\+\|\s\+\%#\@<!$/
endfunction

" default indentation scheme
call WhitespaceTab(4)

" linux code looks really ugly without 8-sized tabs
autocmd BufRead /usr/src/linux* call WhitespaceTab(8)

cmap ws2 call WhitespaceSpace(2)
cmap ws4 call WhitespaceSpace(4)
cmap ws8 call WhitespaceSpace(8)
cmap wt2 call WhitespaceTab(2)
cmap wt4 call WhitespaceTab(4)
cmap wt8 call WhitespaceTab(8)

" language-specific stuff
" python-specific
au FileType python setlocal expandtab

" filetypes where whitespaces are preferred
autocmd FileType python,perl,pyrex call WhitespaceSpace(4)
autocmd FileType lisp call WhitespaceSpace(2)

