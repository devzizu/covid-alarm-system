-module(zeromq_servers).
-export([request_pub_notification_central/2 ,request_district_router_central/3,start_zeromq_rep_central/1, start_zeromq_rep_district/2]).

request_pub_notification_central(ClientSocket, Socket) ->
    io:format("\t[Init] Requesting to central server: init_pub~n"),    
    ok = chumak:send(Socket, "init_pub"),
    {ok, RecMessage} = chumak:recv(Socket),
    io:format("\t[Init] Central server said: ~s~n", [RecMessage]),
    MsgAux = binary:bin_to_list(RecMessage, {0, byte_size(RecMessage)}),
    Res = string:tokens(MsgAux,"_\r\n"),
    case length(Res) of
        3 ->
            ["centralserver", "ok", ZMQ_PUB_PORT] = Res,
                % returning received ports
                SendMessage = lists:flatten(io_lib:format("\"pub_~s~n\"", [ZMQ_PUB_PORT])),
                gen_tcp:send(ClientSocket, SendMessage),
                {ok};
        2 ->
            ["centralserver", "error"] = Res,
                gen_tcp:send(ClientSocket, "Não foi possível conectar às notificações públicas, tente mais tarde."),
                {error}
    end.


request_district_router_central(ClientSocket, Socket, Username) ->
    % get user origin and ask for district server
    {ok, Residencia} = login_manager:get_residencia(Username),
    SendMessage = lists:flatten(io_lib:format("\"cliente_~s_~s\"", [Username, Residencia])),
    io:format("\t[~s] Requesting to central server: ~s~n", [Username, SendMessage]),    
    ok = chumak:send(Socket, SendMessage),
    {ok, RecMessage} = chumak:recv(Socket),
    io:format("\t[~s] Central server said: ~s~n", [Username, RecMessage]),
    MsgAux = binary:bin_to_list(RecMessage, {0, byte_size(RecMessage)}),
    Res = string:tokens(MsgAux,"_\r\n"),
    case length(Res) of
        3 ->
            ["centralserver", "ok", ZMQ_DISTRICT_PORT] = Res,
                % returning received ports
                {ok, list_to_integer(ZMQ_DISTRICT_PORT)};
        2 ->
            ["centralserver", "error"] = Res,
                gen_tcp:send(ClientSocket, "Nenhum servidor disponível, tente mais tarde."),
                {error}
    end.

start_zeromq_rep_central(CS_PORT) ->
    % create socket type REQ for district server requests
    {ok, Socket} = chumak:socket(req),
    % connect to central server port: CS_PORT
    case chumak:connect(Socket, tcp, "localhost", CS_PORT) of
        {ok, _} ->
            io:format("\t[Init] Binding REQ CentralServer - OK with Pid: ~p\n", [Socket]);
        {error, Reason} ->
            io:format("[Init] Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("\t[Init] Unhandled reply for bind ~p \n", [X])
    end,
    Socket.

 start_zeromq_rep_district(ZMQ_DISTRICT_PORT, Username) ->
    % create socket type REQ for district server requests
    {ok, Socket} = chumak:socket(req),
    case chumak:connect(Socket, tcp, "localhost", ZMQ_DISTRICT_PORT) of
        {ok, _} ->
            io:format("[~s] Binding OK with Pid: ~p\n", [Username, Socket]);
        {error, Reason} ->
            io:format("[~s] Connection Failed for this reason: ~p\n", [Username, Reason]);
        X ->
            io:format("[~s] Unhandled reply for bind ~p\n", [Username, X])
    end,
    Socket.