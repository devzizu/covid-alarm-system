-module(zeromq_servers).
-export([request_ports_central/3, start_zeromq_push/2, start_zeromq_sub/2, send_backend/3, receive_zeromq_sub/2]).

request_ports_central(ClientSocket, CS_PORT, Username) ->
    % create socket type REQ for central server requests
    {ok, Socket} = chumak:socket(req, "central_server"),
    % connect to central server port: CS_PORT
    case chumak:connect(Socket, tcp, "localhost", CS_PORT) of
        {ok, _} ->
            io:format("\t[~s] Binding REQ CentralServer - OK with Pid: ~p\n", [Username, Socket]);
        {error, Reason} ->
            io:format("[~s] Connection Failed for this reason: ~p\n", [Username, Reason]);
        X ->
            io:format("\t[~s] Unhandled reply for bind ~p \n", [Username, X])
    end,
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
        4 ->
            ["centralserver", "ok", ZMQ_PUSH_PORT, ZMQ_XPUB_PORT] = Res,
                % returning received ports
                {ok, ZMQ_PUSH_PORT, ZMQ_XPUB_PORT};
        2 ->
            ["centralserver", "error"] = Res,
                gen_tcp:send(ClientSocket, "Nenhum servidor disponível, tente mais tarde."),
                {error}
    end.
            
start_zeromq_push(ZMQ_PUSH_PORT, Username) ->
    % create socket type PUSH for district server requests
    {ok, Socket} = chumak:socket(push),
    case chumak:connect(Socket, tcp, "localhost", ZMQ_PUSH_PORT) of
        {ok, _} ->
            io:format("[~s] Binding OK with Pid: ~p\n", [Username, Socket]);
        {error, Reason} ->
            io:format("[~s] Connection Failed for this reason: ~p\n", [Username, Reason]);
        X ->
            io:format("[~s] Unhandled reply for bind ~p\n", [Username, X])
    end,
    Socket.

start_zeromq_sub(ZMQ_XPUB_PORT, Username) ->
    % create socket type SUB for incoming notifications
    {ok, Socket} = chumak:socket(sub),
    SUBTOPIC =  lists:flatten(io_lib:format("~s_", [Username])),
    chumak:subscribe(Socket, SUBTOPIC),
    case chumak:connect(Socket, tcp, "localhost", ZMQ_XPUB_PORT) of
        {ok, _} ->
            io:format("[~s] Binding OK with Pid: ~p\n", [Username, Socket]);
        {error, _} ->
            io:format("[~s] Connection Failed for this reason: (omitted)\n", [Username]);
        X ->
            io:format("[~s] Unhandled reply for bind ~p \n", [Username, X])
    end,
    Socket.

% Handle pushs and subs

send_backend(Username, Socket_Push, OperationData) ->
    RequestOp = lists:flatten(io_lib:format("~s_~s", [Username, OperationData])),
    io:format("[~s] Pushing operation ~s \n", [Username, RequestOp]),
    ok = chumak:send(Socket_Push, RequestOp).

receive_zeromq_sub(Socket_tcp_client, Socket_Sub) ->
    {ok, RecMessage} = chumak:recv(Socket_Sub),
    gen_tcp:send(Socket_tcp_client, RecMessage),
    receive_zeromq_sub(Socket_tcp_client, Socket_Sub).