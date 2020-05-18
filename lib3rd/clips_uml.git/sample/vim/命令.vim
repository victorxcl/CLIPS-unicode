:call remote_send('VIM', ':call Iapi_hello("好呀")<CR>')

:call remote_expr('VIM', 'Iapi_hello("我好呀")', 'serverid')
:echo remote_read(serverid)

:autocmd CursorMoved <buffer> :echo getpos('.')
