syntax on
set t_Co=256
set et
set sw=4
set smarttab
set tw=79
set tabpagemax=25
set pastetoggle=<F2>
colorscheme elflord
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal g'\"" | endif
endif


" Uncomment the following to have Vim load indentation rules according to the
" detected filetype. Per default Debian Vim only load filetype specific
" plugins.
if has("autocmd")
  filetype indent on
endif

set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set autowrite		" Automatically save before commands like :next and :make
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif
set foldmethod=syntax
