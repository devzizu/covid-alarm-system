-module(login_manager).
-export([start/0, create_account/3, close_account/2, login/3, logout/1, rpc/1, loop/1, get_residencia/1, get_socket/1]).

%MAP -> {[key = Username], [value = {password, is_logged ?, Residencia}]

start() ->
	Pid = spawn(fun() -> loop(#{}) end),
	register(?MODULE, Pid).

%Server Loop
%Accounts é o map
loop(Accounts) ->
	receive 
		{{create_account, Username, Passwd, Residencia}, From} -> 
		 case maps:find(Username, Accounts) of
		 	error -> 
				From ! {ok}, 
				loop(maps:put(Username, {Passwd, false, Residencia, null, false}, Accounts)); 
		 	_ ->
				From ! {user_exists}, 
				loop(Accounts)
		 end;

		 {{login, Username, Passwd, Socket}, From} ->
			case maps:find(Username, Accounts) of
				{ok, {Passwd,false,Residencia, _,false}} ->
					From ! {ok},
					loop(maps:update(Username, {Passwd, true, Residencia, Socket, false}, Accounts));
				{ok, {Passwd,false,_, _,true}} ->
					From ! {you_should_be_isolated},
					loop(Accounts);
				_ ->
					From ! {invalid}, 
					loop(Accounts)
			end;

		 {{logout, Username}, From} ->
			case maps:find(Username, Accounts) of
				{ok, {Passwd,true,Residencia, _}} ->
					From ! {ok},
					% logout = infected
					loop(maps:update(Username, {Passwd, false, Residencia, true}, Accounts));
				_ ->
					From ! {invalid}, 
					loop(Accounts)
			end;
		{{close_account, Username, Passwd}, From} ->
			case maps:find(Username, Accounts) of
				{ok, {Passwd, _}} ->
					From ! {ok},
					loop(maps:remove(Username, Accounts));
				_ -> 
					From ! {invalid}, 
					loop(Accounts)
			end;
		{{get_residencia, Username}, From} -> 
			case maps:find(Username, Accounts) of
				{ok, {_, true, Residencia,_, _}} ->
					From ! {ok, Residencia};
				_-> 
					From ! {invalid}
			end,
			loop(Accounts);
		{{get_socket, Username}, From} -> 
			case maps:find(Username, Accounts) of
				{ok, {_, true, _, S, _}} ->
					From ! {ok, S};
				_-> 
					From ! {invalid}
			end,
			loop(Accounts)
	end.

rpc(Request)-> 
	?MODULE ! {Request, self()},
	receive
		Res -> Res
	end.

get_residencia(Username) -> rpc({get_residencia, Username}).

create_account(Username, Passwd, Residencia)-> rpc({create_account, Username, Passwd, Residencia}).

close_account(Username, Passwd)-> rpc({close_account, Username, Passwd}).

login(Username, Passwd, Socket)-> rpc({login, Username, Passwd, Socket}).

logout(Username)-> rpc({logout, Username}).

get_socket(Username) -> rpc({get_socket, Username}).