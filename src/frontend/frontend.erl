-module(frontend).
-export([start_frontend/0, acceptor/1, start_zeromq_servers/1]).

start_frontend() ->
    {ok, LSock} = gen_tcp:listen(FRONTEND_PORT, [{active, once}, {packet, line},
                                        {reuseaddr, true}]),
    application:start(chumak),
    login_manager:start(),
    spawn(fun() -> acceptor(LSock) end),
    ok.

acceptor(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    {ok, Username} = enter_app(Socket),
    start_zeromq_servers(Username),
    handle_client_socket(Socket, Username).

handle_client_socket(Socket, User) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        % mandar servidores destritais zeromq
        {tcp, _, Data} ->  
            io:format("~p~n",[Data]),
            send_backend(),
            handle_client_socket(Socket, User);
        % logout
        {tcp_closed, _} ->
            io:format("~p~n",["closed"]);
        % logout
        {tcp_error, _, _} ->
            io:format("~p~n",["error"])
    end.

enter_app(Socket) ->
    receive
        {tcp, _, Data} ->
            inet:setopts(Socket, [{active, once}]),
            Tokens = string:tokens(Data," \r\n"),
            case length(Tokens) of
                % [login, <user>, <pass>]
                3 ->              
                    ["login", Username, Password] = Tokens,
                    case login_manager:login(Username, Password) of
                        { ok } -> UserLogged = {ok, Username};
                        _ -> UserLogged = {null, invalid}
                    end;
                % [login, <user>, <pass>, <residencia>]
                4 -> 
                    ["create", Username, Password, Residencia] = Tokens,          
                    case login_manager:create_account(Username, Password, Residencia) of
                        { ok } -> UserLogged = {null, login_pls};
                        _ -> UserLogged = {null, exists}
                    end;
                _ -> UserLogged = {null, invalid_msg}
            end
    end,
    Msg = lists:flatten(io_lib:format("~p\n", [UserLogged])),
    gen_tcp:send(Socket, Msg),
    case UserLogged of
        {null,_} -> enter_app(Socket);
        {ok,_} -> UserLogged
    end.


send_backend() ->
true.

start_zeromq_push() ->
    {ok, Socket} = chumak:socket(push),
    case chumak:connect(Socket, tcp, "localhost", ZMQ_PUSH_PORT) of
        {ok, _BindPid} ->
            io:format("[~p] Binding OK with Pid: ~p\n", [Username, Socket]);
        {error, Reason} ->
            io:format("[~p] Connection Failed for this reason: (omitted)\n", [Username]);
        X ->
            io:format("[~p] Unhandled reply for bind ~p \n", [X])
    end,
    Socket.

start_zeromq_sub() ->


start_zeromq_servers(Username) ->
    {ok, Residencia} = login_manager:get_residencia(Username),

    .