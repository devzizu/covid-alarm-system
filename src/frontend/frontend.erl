-module(frontend).

-export([acceptor/2, handle_requests_req/3, start_frontend/0, handle_private_notifications/1]).

-define(CONFIG_FILE, "../config.toml").

start_frontend() ->
    io:format("Reading configuration file ~s.~n",
              [?CONFIG_FILE]),
    % read configuration file
    {ok, Configuration} = toml:read_file(?CONFIG_FILE),
    {integer, FE_PORT} = toml:get_value(["ports"],
                                        "FRONTEND",
                                        Configuration),
    {integer, FE_PULL} = toml:get_value(["ports"],
                                        "FRONTEND_PULL",
                                        Configuration),
    io:format("Frontend is listenning on port (tcp): ~p.~n", [FE_PORT]),
    SocketPull = zeromq_servers:start_pull(FE_PULL),
    % bind listen port to the frontend server
    io:format("Frontend is listenning on port (pull): ~p.~n",
              [FE_PULL]), 
    {ok, LSock} = gen_tcp:listen(FE_PORT,
                                 [{active, once},
                                  {packet, line},
                                  {reuseaddr, true}]),
    % start chumak zeromq
    application:start(chumak),
    % start login manager
    login_manager:start(),
    io:format("Login manager and Chumak zeromq started.~n", []),
    spawn(fun () -> acceptor(LSock, Configuration) end),
    handle_private_notifications(SocketPull),
    ok.

handle_private_notifications(SocketPull) ->
    {ok, Data} = chumak:recv(SocketPull),
    MsgAux = binary:bin_to_list(Data, {0, byte_size(Data)}),
    [Username|_] = string:tokens(MsgAux,"_\r\n"),
    {ok, SocketClient} = login_manager:get_socket(Username),
    io:format("\t[~s] private notification: ~s for socket: ~p~n", [Username, MsgAux, SocketClient]),
    %...ver qual é o user e buscar o socket
    SendMessage = lists:flatten(io_lib:format("~s~n", [MsgAux])),
    gen_tcp:send(SocketClient, SendMessage),
    handle_private_notifications(SocketPull).

acceptor(LSock, Configuration) ->
    {ok, ClientSocket} = gen_tcp:accept(LSock),
    spawn(fun () -> acceptor(LSock, Configuration) end),
    io:format("\tAccepted new client...~n", []),
    % Get ports from REQ socket: central server
    {integer, CS_PORT} = toml:get_value(["ports"],
                                        "CENTRAL_SERVER_REP",
                                        Configuration),
    % Inicialization of socket Req to communicate to central server
    Socket_Req_central =
        zeromq_servers:start_zeromq_rep_central(CS_PORT),
    % Faz um request ao servidor pela porta pub e envia ao cliente
    zeromq_servers:request_pub_notification_central(ClientSocket,
                                                    Socket_Req_central),
    % Independentemente do resultado pode seguir com o login
    {ok, Username} = enter_app(ClientSocket),
    io:format("\tClient ~p logged in...~n", [Username]),
    %Init comunication with central server -> Response Push port and xSub port
    io:format("\tStarting zeromq servers (req, push, "
              "xpub)...~n",
              []),
    case
        zeromq_servers:request_district_router_central(ClientSocket,
                                                       Socket_Req_central,
                                                       Username)
        of
        {ok, DISTRICT_PORT} ->
            % Sending port to cliente
            Socket_REQ =
                zeromq_servers:start_zeromq_rep_district(DISTRICT_PORT,
                                                         Username),
            handle_requests_req(ClientSocket, Socket_REQ, Username);
        {error} -> io:format("\tCannot start servers!~n", [])
    end.

handle_requests_req(ClientSocket, Socket_Req, Username) ->
    receive
        % mandar servidores destritais zeromq
        {tcp, _, OperationData} ->
            inet:setopts(ClientSocket, [{active, once}]),
            io:format("Sending operation to backend: ~p~n",
                      [OperationData]),
            chumak:send(Socket_Req, OperationData),
            {ok, RecMessage} = chumak:recv(Socket_Req),
            gen_tcp:send(ClientSocket, RecMessage),
            handle_requests_req(ClientSocket, Socket_Req, Username);
        % logout
        {tcp_closed, _} -> io:format("~s~n", ["closed"]);
        % logout
        {tcp_error, _, _} -> io:format("~s~n", ["error"])
    end.


enter_app(Socket) ->
    receive
        {tcp, _, Data} ->
            inet:setopts(Socket, [{active, once}]),
            Tokens = string:tokens(Data, " \r\n"),
            case length(Tokens) of
                % [login, <user>, <pass>]
                3 ->
                    ["login", Username, Password] = Tokens,
                    case login_manager:login(Username, Password, Socket) of
                        {ok} -> UserLogged = {ok, Username};
                        _ -> UserLogged = {null, invalid}
                    end;
                % [login, <user>, <pass>, <residencia>]
                4 ->
                    ["create", Username, Password, Residencia] = Tokens,
                    case login_manager:create_account(Username,
                                                      Password,
                                                      Residencia)
                        of
                        {ok} -> UserLogged = {null, login_pls};
                        _ -> UserLogged = {null, exists}
                    end;
                _ -> UserLogged = {null, invalid_msg}
            end
    end,
    Msg = lists:flatten(io_lib:format("~p\n",
                                      [UserLogged])),
    gen_tcp:send(Socket, Msg),
    case UserLogged of
        {null, _} -> enter_app(Socket);
        {ok, _} -> UserLogged
    end.