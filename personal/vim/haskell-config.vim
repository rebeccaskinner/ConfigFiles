
let g:ghcmod_ghc_opeions = ['-L/usr/lib/slamr', '-lslamr-libcontinuation', '-lslamr-libdb', '-lslamr-liblog', '-lslamr-libnet', '-lslamr-libplugin', '-lslamr-libstd', '-luuid']

hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

autocmd BufWritePost *.hs call s:check_and_lint()
function! s:check_and_lint()
  let l:qflist = ghcmod#make('check')
  call extend(l:qflist, ghcmod#make('lint'))
  call setqflist(l:qflist)
  cwindow
  if empty(l:qflist)
    echo "No errors found"
  endif
endfunction

let g:necoghc_enable_detailed_browse = 1
