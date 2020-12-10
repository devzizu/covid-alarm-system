-module(frontend).
-export([start_frontend/0, acceptor/2, handle_requests_push/3]).

-define(CONFIG_FILE, "../config.toml").

start_frontend() ->
    io:format("Reading configuration file ~s.~n", [?CONFIG_FILE]),
    % read configuration file    
    {ok, Configuration} = toml:read_file(?CONFIG_FILE),
    {integer, FE_PORT} = toml:get_value(["ports"], "FRONTEND", Configuration),
    % bind listen port to the frontend server
    io:format("Frontend is listenning on port: ~p.~n", [FE_PORT]),
    {ok, LSock} = gen_tcp:listen(FE_PORT, [{active, once}, {packet, line},
                                           {reuseaddr, true}]),
    % start chumak zeromq
    application:start(chumak),
    % start login manager
    login_manager:start(),
    io:format("Login manager and Chumak zeromq started.~n", []),
    spawn(fun() -> acceptor(LSock, Configuration) end),
    ok.

acceptor(LSock, Configuration) ->
    {ok, ClientSocket} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Configuration) end),
    io:format("\tAccepted new client...~n", []),
    {ok, Username} = enter_app(ClientSocket),
    io:format("\tClient ~p logged in...~n", [Username]),
    %Init comunication with central server -> Response Push port and xSub port
    io:format("\tStarting zeromq servers (req, push, xpub)...~n", []),
    % Get ports from REQ socket: central server
    {integer, CS_PORT} = toml:get_value(["ports"], "CENTRAL_SERVER_REP", Configuration),
    case zeromq_servers:request_ports_central(ClientSocket, CS_PORT, Username) of
        {ok, PUSH_PORT, XPUB_PORT} -> 
           % Connection to push district server    
            Socket_Push = zeromq_servers:start_zeromq_push(PUSH_PORT, Username),
            % Connection to push xpub dynamic server    
            Socket_SUB = zeromq_servers:start_zeromq_sub(XPUB_PORT, Username),
            % Communication between client and front-end server & Front-end and district server
            io:format("\tHandler for socket subs started...~n", []),
            spawn(fun() -> zeromq_servers:receive_zeromq_sub(ClientSocket, Socket_SUB) end),
            io:format("\tHandler for socket pushs started...~n", []),
            handle_requests_push(ClientSocket, Socket_Push, Username);
            
        {error} ->
            io:format("\tCannot start servers!~n", [])
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

handle_requests_push(ClientSocket, Socket_Push, Username) ->
    receive
        % mandar servidores destritais zeromq
        {tcp, _, OperationData} ->  
            inet:setopts(ClientSocket, [{active, once}]),
            io:format("Sending operation to backend: ~p~n", [OperationData]),
            zeromq_servers:send_backend(Username, Socket_Push, OperationData),
            handle_requests_push(ClientSocket, Socket_Push, Username);
        % logout    
        {tcp_closed, _} ->
            io:format("~s~n",["closed"]);
        % logout
        {tcp_error, _, _} ->
            io:format("~s~n",["error"])
    end.
