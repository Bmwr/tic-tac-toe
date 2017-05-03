-module(game).
-behaviour(gen_fsm).

%%% Public API

%% Behavior functions
-export([start/1, start_link/1]).
%% gen_fsm callbacks
-export([
    % Behaviour callbacks
    init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
    % State functions
    waiting_for_players/2, in_progress/2, in_progress/3
]).
%% Utility functions
-export([get_number/1, add_player/2, get_active_player/1,
    shut_down/1
]).

%%% State Record

-record(?MODULE, {
    number,
    players = [],
    active_player,
    game = {0,0,0,0,0,0,0,0,0},
    turn =0
}).

%%% Public API

%% Behavior functions

start(Number) ->
    gen_fsm:start(?MODULE, [Number], []).

start_link(Number) ->
    gen_fsm:start_link(?MODULE, [Number], []).

%% Behavior callbacks

init([Number]) ->
    random:seed(erlang:now()),
    {ok, waiting_for_players, #?MODULE {number = Number}}.

handle_event(stop, _StateName, Data) ->
    {stop, normal, Data};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName, Data),
    {next_state, StateName, Data}.

handle_sync_event(getnumber, _From, StateName, Data) ->
    {reply, Data#?MODULE.number, StateName, Data};
handle_sync_event(getactiveplayer, _From, StateName, Data) ->
    {reply, Data#?MODULE.active_player, StateName, Data};
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName, Data),
    {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName, Data),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(normal, StateName, State=#?MODULE{}) ->
    io:format("Game terminated normally, state name = ~p, ~p~n", [StateName, State]);
terminate(Reason, StateName, State) ->
    io:format("Game terminated abnormally, reason = ~p, state name = ~p, state = ~p~n", [Reason, StateName, State]).

%% State functions

waiting_for_players({addplayer, Player}, Data) ->
    Game = Data#?MODULE.number,
    io:format("Game ~p, adding player ~p~n", [Game, player:get_name(Player)]),
    NewPlayers = [Player | Data#?MODULE.players],
    PlayerCount = length(NewPlayers),
    gen_fsm:send_event(Player, {gamejoined, self()}),
    AP = maybe_play(PlayerCount,NewPlayers,Data#?MODULE.game),
    {next_state, waiting_for_players(PlayerCount), Data#?MODULE{players = NewPlayers, active_player = AP}};
waiting_for_players(Event, State) ->
    io:format("waiting_for_tournament: received unknown event ~p, state = ~p~n", [Event, State]), {next_state, waiting_for_tournament, State}.

in_progress({playeraction, Player, {Row, Column}}, State = #?MODULE{active_player = Player, game = Game , players =  Players, turn = Turn}) ->
    io:format("in_progress: Player ~p did ~p~n", [Player, {Row, Column}]),
    Index = index(Column,Row),
    case is_valid_action(Index,Game) of
        true ->
            _NewGame = setelement(Index,Game,Player),
            _CheckWinner = check_winner(_NewGame,Turn+1),
            in_progress_valid_action(_CheckWinner, Turn+1, Player, new_player(Players,Player), _NewGame, State);
        _ ->
            in_progress_invalid_action(Player,{Row,Column},State)
    end;
in_progress({playeraction, Player, _}, State) ->
    gen_fsm:send_event(Player,notYourTurn),
    {next_state, in_progress,State};

in_progress(Event, State) ->
    io:format("in_progress: received unknown event ~p, state = ~p~n", [Event, State]), {next_state, in_progress, State}.

in_progress(Event, From, State) ->
    io:format("in_progress: received unknown event ~p from ~p, state = ~p~n", [Event, From, State]), {next_state, in_progress, State}.

%% Utility functions

get_number(Game) ->
gen_fsm:sync_send_all_state_event(Game, getnumber).

get_active_player(Game) ->
    gen_fsm:sync_send_all_state_event(Game, getactiveplayer).

add_player(Game, Player) ->
    gen_fsm:send_event(Game, {addplayer, Player}).

shut_down(Game) ->
    gen_fsm:send_all_state_event(Game, stop).

%%% Private Functions
in_progress_valid_action(none, NewTurn, OldP, NewP, Game, State) ->
    gen_fsm:send_event(NewP, {yourmove, Game}),
    gen_fsm:send_event(OldP, gotmove),
    {next_state, in_progress, State#?MODULE{game = Game, active_player = NewP, turn = NewTurn}};
in_progress_valid_action(par, _, OldP, NewP, _, State) ->
    gen_fsm:send_event(NewP, {gameover, par}),
    gen_fsm:send_event(OldP, {gameover, par}),
    {stop,normal,State};
in_progress_valid_action(Win, _, _, _, _, State = #?MODULE{players = Players}) ->
    gen_fsm:send_event(Win, {gameover, winner}),
    gen_fsm:send_event(new_player(Players,Win), {gameover, looser}),
    {stop,normal,State}.

in_progress_invalid_action(Player,Action,State) ->
    gen_fsm:send_event(Player, {badaction, Action}),
    {next_state, in_progress,State}.

index(C, R) when ((C =:= 1) orelse (C =:= 2) orelse (C =:= 3)) andalso
                 ((R =:= 1) orelse (R =:= 2) orelse (R =:= 3)) ->
    C + (R - 1) * 3;
index(_,_) -> error.

is_valid_action(I,Game) when is_integer(I) ->
    element(I,Game) == 0;
is_valid_action(_,_) -> false.

new_player([P1,P2],P1) -> P2;
new_player([P1,_],_) -> P1.

maybe_play(1,_,_) -> none;
maybe_play(2,[P1,P2],Game) ->
    S = random:uniform(2),
    P = case S of
        1 -> P1;
        2 -> P2
    end,
    yourmove(P,Game),
    P.

yourmove(P,Game) ->
    gen_fsm:send_event(P, {yourmove, Game}).

waiting_for_players(1) -> waiting_for_players;
waiting_for_players(2) -> in_progress.


check_winner({X,X,X,_,_,_,_,_,_},_) when X =/= 0 -> X;
check_winner({_,_,_,X,X,X,_,_,_},_) when X =/= 0 -> X;
check_winner({_,_,_,_,_,_,X,X,X},_) when X =/= 0 -> X;
check_winner({X,_,_,X,_,_,X,_,_},_) when X =/= 0 -> X;
check_winner({_,X,_,_,X,_,_,X,_},_) when X =/= 0 -> X;
check_winner({_,_,X,_,_,X,_,_,X},_) when X =/= 0 -> X;
check_winner({X,_,_,_,X,_,_,_,X},_) when X =/= 0 -> X;
check_winner({_,_,X,_,X,_,X,_,_},_) when X =/= 0 -> X;
check_winner(_,9) -> par;
check_winner(_,_) -> none.

%% Helper Functions

unexpected(Msg, State, Data) ->
    io:format("~p received unknown event ~p while in state ~p, Data = ~p~n", [self(), Msg, State, Data]).
