""" vim: autoindent expandtab

:function! Tapi_response(bufnr, json)
:   call server2client(a:json.client, json_encode(a:json))
:endfunction

:function! Iapi_hello(world)
:   let  [CLIPS, clientid] = [term_list()[0], expand('<client>')]
:   call term_sendkeys(CLIPS, printf("(assert (测试 %s %s))\<CR>", clientid, a:world))
:   call term_sendkeys(CLIPS, printf("(run)\<CR>"))
:endfunction 
