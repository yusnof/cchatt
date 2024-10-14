-module(server).
-export([start/1, stop/1]).

-record(serverState, {
    users,
    channels
}).

initial_state() ->
    #serverState{
        users = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

handle(State, {join, Pid, NewUserName, Channel}) ->
    case lists:member({NewUserName, Pid}, State#serverState.users) of
        true ->
            {reply, {error, already_in_channel}, State};
        false ->
            %% Add the user to the channel
            NewUsers = [{NewUserName, Pid} | State#serverState.users],
            NewState = State#serverState{users = NewUsers},
            {reply, {ok, Channel}, NewState}
    end;


%function that handles the quitComand and that replyes with ok
% and with the state without the deleted user
handle(State, {quit, UserName}) ->
    TempList = lists:delete(UserName, State#serverState.users),
    {reply, ok, State#serverState{users = TempList}}; 

%function that handles changing NickName 
handle(State,{nick, OldUserName, NewUserName})-> 
    case lists:member(NewUserName, State#serverState.users) of
        true when OldUserName =:= NewUserName ->
            {reply, ok, State};
        false ->
            TempList = [NewUserName | lists:delete(OldUserName, State#serverState.users)],
            {reply, ok, State#serverState{users=TempList}}
        end;


%ToDo catch other commands with some type of exeption
handle(State, _ ) ->
    {reply, {error, not_implemented}, State}.