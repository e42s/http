-module(simple_client).

-export([test/0]).


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


request(Address, Port, Method, Host, Path, Data) ->
    {ok, Conn} =
        gen_tcp:connect(
          Address, Port,
          [binary,
           {packet, http_bin},
           {active, false}]),
    ok =
        gen_tcp:send(
          Conn,
          [Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>,
           <<"Host: ">>, Host, <<"\r\n">>,
           <<"Content-Length: ">>, integer_to_list(iolist_size(Data)), <<"\r\n">>,
           <<"\r\n">>, Data]),

    {ok, {http_response, _Version, _Code, _Reason} = Response} =
        gen_tcp:recv(Conn, 0),
    Headers = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),
    Length = binary_to_integer(proplists:get_value('Content-Length', Headers)),
    {ok, Body} = gen_tcp:recv(Conn, Length),
    gen_tcp:close(Conn),

    {Response, Headers, Body}.


request(Address, Port, Method, Host, Path) ->
    request(Address, Port, Method, Host, Path, <<>>).


test(simple_server) ->
    {{http_response, _, 200, _}, _, <<"It works!">>} =
        request("127.0.0.1", 8000, <<"GET">>, <<"localhost">>, <<"/">>),
    ok;
test(simple_file_server) ->
    Addr = "127.0.0.1",
    Port = 8080,
    Host = <<"localhost">>,
    Path = <<"/a.txt">>,
    Content1 = <<"1">>,
    Content2 = <<"2">>,
    GET = <<"GET">>,
    PUT = <<"PUT">>,
    DELETE = <<"DELETE">>,

    {{http_response, _, 404, _}, _, _} =
        request(Addr, Port, GET, Host, Path),

    {{http_response, _, 404, _}, _, _} =
        request(Addr, Port, DELETE, Host, Path),

    {{http_response, _, 200, _}, _, _} =
        request(Addr, Port, PUT, Host, Path, Content1),

    {{http_response, _, 200, _}, _, Content1} =
        request(Addr, Port, GET, Host, Path),

    {{http_response, _, 200, _}, _, _} =
        request(Addr, Port, PUT, Host, Path, Content2),

    {{http_response, _, 200, _}, _, Content2} =
        request(Addr, Port, GET, Host, Path),

    {{http_response, _, 200, _}, _, _} =
        request(Addr, Port, DELETE, Host, Path),

    {{http_response, _, 404, _}, _, _} =
        request(Addr, Port, GET, Host, Path),

    {{http_response, _, 404, _}, _, _} =
        request(Addr, Port, DELETE, Host, Path),

    ok.


test() ->
    test(simple_server),
    test(simple_file_server),
    ok.
