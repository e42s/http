-module(simple_file_server).

-export([start/0]).

-export([init/1, loop/1, handle_connection/2]).


start() ->
    spawn(?MODULE, init, [8080]).


init(Port) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http_bin},
           {active, false},
           {reuseaddr, true}]),
    ?MODULE:loop(Socket).


loop(Socket) ->
    case gen_tcp:accept(Socket, 100) of
        {ok, Conn} ->
            Ref = make_ref(),
            Pid = spawn(?MODULE, handle_connection, [Ref, Conn]),
            ok = gen_tcp:controlling_process(Conn, Pid),
            Pid ! Ref;
        {error, timeout} ->
            ok
    end,
    ?MODULE:loop(Socket).


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


handle_connection(Ref, Conn) ->
    receive
        Ref ->
            {ok, {http_request, Method, {abs_path, Path}, Version}} = gen_tcp:recv(Conn, 0),

            Headers = recv_headers(Conn),
            ok = inet:setopts(Conn, [{packet, raw}]),

            try
                Host = proplists:get_value('Host', Headers),
                handle_request(Conn, Method, Host, Path, Version, Headers)
            after
                ok = gen_tcp:close(Conn)
            end
    after 1000 ->
            throw(timeout)
    end.


response(Conn, Code, ContentType, Headers, Body) ->
    ok =
        gen_tcp:send(
          Conn,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           <<"Content-Length: ">>, integer_to_list(iolist_size(Body)), <<"\r\n">>,
           Headers,
           <<"\r\n">>, Body]).


handle_request(Conn, 'GET', _Host, <<"/", Path/binary>>, _Version, _Headers) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            response(Conn, 200, <<"application/octet-stream">>, [], Bin);
        {error, _} ->
            response(Conn, 404, <<"application/octet-stream">>, [], <<"Not Found">>)
    end;            
handle_request(Conn, 'PUT', _Host, <<"/", Path/binary>>, _Version, Headers) ->
    Length = binary_to_integer(proplists:get_value('Content-Length', Headers)),
    {ok, Body} = gen_tcp:recv(Conn, Length),
    ok = file:write_file(Path, Body),
    response(Conn, 200, <<"text/plain">>, [], <<"OK">>);
handle_request(Conn, 'DELETE', _Host, <<"/", Path/binary>>, _Version, _Headers) ->
    case file:delete(Path) of
        ok ->
            response(Conn, 200, <<"text/plain">>, [], <<"OK">>);
        {error, _} ->
            response(Conn, 404, <<"application/octet-stream">>, [], <<"Not Found">>)
    end.
