-module(login_manager).
-export([start/0, create_account/3, close_account/2, login/2, rpc/1, loop/1, get_residencia/1]).

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
				loop(maps:put(Username, {Passwd, false, Residencia}, Accounts)); 
		 	_ ->
				From ! {user_exists}, 
				loop(Accounts)
		 end;

		 {{login, Username, Passwd}, From} ->
			case maps:find(Username, Accounts) of
				{ok, {Passwd,false,Residencia}} ->
					From ! {ok},
					loop(maps:update(Username, {Passwd, true, Residencia}, Accounts));
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
				{ok, {_, true, Residencia}} ->
					From ! {ok, Residencia};
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

login(Username, Passwd)-> rpc({login, Username, Passwd}).
