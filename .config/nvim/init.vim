" Show line numbers
set number
" Use mouse
set mouse=a
" Enable syntax highlighting
syntax on

" Encoding, tabs, smartcase, etc
set encoding=UTF-8
set tabstop=4
set shiftwidth=4
set noexpandtab
set sw=4
set termguicolors
set smartcase

" This is only necessary if you use "set termguicolors".
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
if has('gui_running')
	set guifont=Hack:h12
	set guioptions-=L
	set guioptions-=L
endif

" Make backspace work normally
set backspace=2

" Plugins
let g:plug_shallow=1
call plug#begin()

" Better status line
Plug 'itchyny/lightline.vim'

" ChadTree
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': ':UpdateRemotePlugins'}

" Add icons to CHADTree
Plug 'ryanoasis/vim-devicons' 

" Color scheme
Plug 'ghifarit53/tokyonight-vim'

" Close braces, quotes etc
Plug 'jiangmiao/auto-pairs'

" Autocomplete
Plug 'ycm-core/YouCompleteMe' 

" Show open buffers on top
Plug 'zefei/vim-wintabs'
Plug 'zefei/vim-wintabs-powerline'

" Local VimRC for projects
Plug 'LucHermitte/lh-vim-lib'
Plug 'LucHermitte/local_vimrc'

" CtrlP for search (basically a lighter fzf)
Plug 'ctrlpvim/ctrlp.vim'

" Debugging
Plug 'puremourning/vimspector', { 'for': ['python', 'cpp', 'c'] }

" Sneak for moving around quickly
Plug 'justinmk/vim-sneak'

" Asynchronous running
Plug 'skywind3000/asyncrun.vim'

" Surround stuff
Plug 'tpope/vim-surround'

" Repeat the last command with '.', not the last 'native' command
Plug 'tpope/vim-repeat'

" LaTeX live preview
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

call plug#end()

" Command assist
:set wildmenu

" For running python files
" autocmd FileType python nnoremap <buffer> <F9> :w <bar> :silent !python shellescape(@%, 1) & pause<CR>

" XX to quit
nnoremap XX :qa<CR>

" Autoindent
set autoindent

" :W to save as well
command W w

" Color scheme
let g:tokyonight_enable_italic = 0
let g:tokyonight_transparent_background = 1
let g:tokyonight_disable_italic_comment = 1
colorscheme tokyonight

" Don't show --insert-- (because we have lightline)
set noshowmode

" YCM autoload config file
let g:ycm_confirm_extra_conf = 0

" Autocomplete after typing 1 character
let g:ycm_semantic_triggers =  {
			\   'c,cpp,cs,python': [ 're!\w{1}', '_' ],
			\ }

"Disable preview on top thing
set completeopt-=preview

" Use Ctrl+Arrows to switch between buffers, ignore quickfix
function! BSkipQuickFix(command)
	update
	let start_buffer = bufnr('%')
	execute a:command
	while &buftype ==# 'quickfix' && bufnr('%') != start_buffer
		execute a:command
	endwhile
endfunction

" nnoremap <C-K> :call BSkipQuickFix("WintabsNext")<CR>
" nnoremap <C-J> :call BSkipQuickFix("WintabsPrevious")<CR>
nnoremap <silent> <C-K> :WintabsNext<CR>
nnoremap <silent> <C-J> :WintabsPrevious<CR>

" Source localvimrc everywhere
call lh#local_vimrc#munge('whitelist', '/*')
let g:local_vimrc_options['asklist'] = [] 

" CtrlP to open CtrlP for files
nnoremap <silent> <C-P> :CtrlP<CR>

" CtrlF to open CtrlP for lines in current buffer
nnoremap <silent> <C-F> :CtrlPLine<CR>

" Close current buffer using ff
nnoremap <silent> ff :WintabsClose<CR>

" Force close current buffer using FF
nnoremap <silent> FF :WintabsClose!<CR>

" Use sytem clipboard for copy/paste/yank/whatever
set clipboard=unnamedplus

" Use 'C' and 'x' for actual delete
nnoremap <silent> C "_dd
nnoremap <silent> x "_x


" Use human mappings for vimspector
let g:vimspector_enable_mappings = 'HUMAN'

" Show hidden files in CtrlP
let g:ctrlp_show_hidden=1

" Stupid option in autopairs disabl e
let g:AutoPairsMultilineClose=0

" Stop vimspector session with :VR
command VR VimspectorReset


" CHADTree toggling
function ToggleCT()
	let w:qfo=0
	for winnr in range(1, winnr('$'))
		if getwinvar(winnr, '&syntax') == 'qf'
			let w:qfo=1
			break
		endif
	endfor
	if w:qfo==1
		cclose
		CHADopen --nofocus
		sleep 35m
		copen
		wincmd p
	else
		CHADopen --nofocus
	endif
endfunction

" Toggle CHADTree
nnoremap <silent> <S-K> :call ToggleCT()<CR>

" Black hole register
nnoremap <silent> <space> "_

" Writing and programming modes
function WritingMode()
	set linebreak
	echo "Writing mode"
	setlocal spell
endfunction

function ProgrammingMode()
	set nolinebreak
	echo "Programming mode"
	setlocal nospell
endfunction

nnoremap <silent> <Leader>w :call WritingMode()<CR>
nnoremap <silent> <Leader>p :call ProgrammingMode()<CR>

" :noh shortcut
nnoremap <silent> <Leader>n :noh<CR>

" LaTeX Live preview: use Zathura instead of okular
let g:livepreview_previewer = 'zathura'

" Spawn split windows on the right and bottom 
set splitbelow
set splitright

" Visual navigation on up/down
autocmd BufEnter * noremap <silent> k gk
autocmd BufEnter * noremap <silent> j gj
autocmd BufEnter * inoremap <silent> <Up> <C-o>gk
autocmd BufEnter * inoremap <silent> <Down> <C-o>gj
noremap <silent> k gk
noremap <silent> j gj
inoremap <silent> <Up> <C-o>gk
inoremap <silent> <Down> <C-o>gj

" Lightline
let g:lightline = {
	  \ 'colorscheme': 'one',
      \ 'separator': { 'left': '', 'right': '' },
  	  \ 'subseparator': { 'left': '', 'right': '' } }
