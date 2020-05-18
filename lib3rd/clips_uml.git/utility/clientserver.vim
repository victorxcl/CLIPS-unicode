""" vim: autoindent expandtab
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:function! VIM_qall()
:   qall!
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:function! BufWithCmd(command)
:   for term in term_list()
:       let info = job_info(term_getjob(term))
:       if info.cmd[0] == a:command
:           return term
:       endif
:   endfor
:   return 0
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:function! CLIPS(command)
:   call term_sendkeys(BufWithCmd('clips'), printf("%s\<CR>", a:command))
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:function! CLIPS_start(clips)
:   execute printf('terminal %s', a:clips)
:endfunction
:function! CLIPS_restart()
:   let  term = BufWithCmd('clips')
:   let  info = job_info(term_getjob(term))
:   call term_sendkeys(term, "(exit)\<CR>")
:   call term_wait(term)
:   call win_execute(bufwinid(term), join(['terminal','++curwin']+info.cmd))
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 在服务端执行
:function! Iapi_request(vim, fact)
:   let [_,protocol;rest] = split(a:fact)
:   let  slots = '' | if exists('*'.protocol) | let slots = {protocol}() | endif
:   let  fmt = '(progn (modify (assert %s)(vim:header %s)(vim:client %s)%s) (run))'
:   call CLIPS(printf(fmt, a:fact, a:vim, expand('<client>'), slots))
:endfunction
:function! Iapi_push(vim, fact)
:   call CLIPS(printf('(progn (assert %s) (run))', a:fact))
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 在客户端执行
:function! Tapi_request(bufnr, json)
:   let  [server, fact] = a:json
:   let  request = printf("Iapi_request('%s', '%s')", v:servername, fact)
:   call remote_expr(server, request, 'serverid')
:   call CLIPS(printf("(progn (assert %s) (run))", remote_read(serverid)))
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 在服务端执行
:function! Tapi_respond(bufnr, json)
:   let  [client, fact] = a:json
:   call server2client(client, fact)
:endfunction
:function! Tapi_push(bufnr, json)
:   let  [client, fact] = a:json
:   call remote_expr(client, printf("Iapi_push('%s', '%s')", v:servername, fact))
:endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:let s:Something = ""
:function! Tapi_store_something(bufnr, something)
:   echomsg a:something
:   let s:Something = a:something->substitute('<CR>',"\n", 'g')
:   echomsg s:Something
:endfunction

:function! Iapi_list_something(query)
"   '用户 account password online'
:   call CLIPS(printf("(list %s)", a:query))
:endfunction

:function! Iapi_return_something()
:   return s:Something
:endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
