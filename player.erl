-module(player).
-behaviour(gen_fsm).

%%% Public API

%% Behavior functions
-export([start/1, start_link/1]).
%% gen_fsm callbacks
-export([
  % Behaviour callbacks
  init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
  % State functions
  waiting_for_game/2, in_game/2, in_game/3
]).
%% Utility functions
-export([
  get_name/1, quit/1, game_quit/1, make_move/3
]).

%%% State Record

-record(?MODULE, {
    user,
    game = none
   }).


%% Behavior functions

start(User) ->
  gen_fsm:start(?MODULE, [User], []).

start_link(User) ->
  gen_fsm:start_link(?MODULE, [User], []).

%% Behavior callbacks

init([User]) ->
  {ok, waiting_for_game, new(User)}.

handle_event(stop, _StateName, Data) ->
  {stop, normal, Data};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName, Data),
  {next_state, StateName, Data}.

handle_sync_event(getname, _From, StateName, Data) ->
  {reply, Data#?MODULE.user, StateName, Data};
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName, Data),
  {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName, Data),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(normal, StateName, State=#?MODULE{}) ->
  io:format("Player terminated normally, state name = ~p, ~p~n", [StateName, State]);
terminate(Reason, StateName, State) ->
  io:format("Player terminated abnormally, reason = ~p, state name = ~p, state = ~p~n", [Reason, StateName, State]).

%% State functions

waiting_for_game({gamejoined, Game}, Data) ->
  io:format("Player ~s: joined game ~p/~p~n", [Data#?MODULE.user, game:get_number(Game), Game]),
  NewState = Data#?MODULE{game = Game},
  {next_state, in_game, NewState};
waiting_for_game(Event, State) ->
  io:format("waiting_for_game: received unknown event ~p, state = ~p~n", [Event, State]),
  {next_state, waiting_for_game, State}.

in_game(gamequit, Data) ->
  Game = Data#?MODULE.game,
  io:format("Player ~s: quit game ~p~n", [Data#?MODULE.user, Game]),
  {next_state, waiting_for_game, Data#?MODULE{game = none}};
in_game({gameover, Winner}, Data) ->
  Game = Data#?MODULE.game,
  Result = case Winner of
      winner -> "you won";
      looser -> "you loose";
      par -> "there is no winner"
  end,
  io:format("~s: ~p Game over, ~s~n", [Data#?MODULE.user, Game, Result]),
  {next_state, waiting_for_game, Data#?MODULE{game = none}};
in_game({yourmove, Game}, State) ->
  print_play(State#?MODULE.user,Game),
  {next_state, in_game, State};
in_game({makemove, Row, Column}, State) ->
  Game = State#?MODULE.game,
  io:format("in_game: setting row ~p, column ~p~n", [Row, Column]),
  gen_fsm:send_event(Game, {playeraction, self(), {Row, Column}}),
  {next_state, in_game, State};
in_game(gotmove, State) ->
  io:format("movement accepted~n"),
  {next_state, in_game, State};
in_game({badaction, Action}, State) ->
  io:format("invalid movement ~p~n",[Action]),
  {next_state, in_game, State};
in_game(notYourTurn, State) ->
  io:format("Please ~s wait for your turn~n",[State#?MODULE.user]),
  {next_state, in_game, State};
in_game(Event, State) ->
  io:format("in_game: received unknown event ~p, state = ~p~n", [Event, State]), {next_state, in_game, State}.

in_game(Event, From, State) ->
  io:format("in_game: received unknown event ~p from ~p, state = ~p~n", [Event, From, State]),
  {next_state, in_game, State}.

%% Utility functions

quit(Player) ->
  gen_fsm:send_all_state_event(Player, stop).

get_name(Player) ->
  gen_fsm:sync_send_all_state_event(Player, getname).

game_quit(Player) ->
  gen_fsm:send_event(Player, gamequit).

make_move(Player, Row, Column) ->
  gen_fsm:send_event(Player, {makemove, Row, Column}).

%%% Private Functions
print_play(P,G) ->
  io:format("~s in_game: it is your turn to play~n~n", [P]),
  Me = self(),
  L = [sign(X,Me) || X <- tuple_to_list(G)],
  print_game(L).

print_game([A,B,C|R]) ->
  io:format("+-+-+-+~n|~c|~c|~c|~n",[A,B,C]),
  print_game(R);
print_game([]) ->
  io:format("+-+-+-+~n~n",[]).

sign(0,_) -> 32;
sign(X,X) -> $X;
sign(_,_) -> $O.

%% Helper Functions

unexpected(Msg, State, Data) ->
  io:format("~p received unknown event ~p while in state ~p, Data = ~p~n", [self(), Msg, State, Data]).

%% Initialization

new(User) ->
  #?MODULE {
   user = lists:flatten(io_lib:format("~s",[User])),
   game = none
  }.
