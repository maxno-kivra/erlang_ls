%%==============================================================================
%% Supervisor for progress-report workers
%%==============================================================================
-module(els_progress_sup).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(supervisor).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ start_child/1
        , start_link/0
        ]).

%% Supervisor Callbacks
-export([ init/1 ]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(els_progress_server:token()) -> ok.
start_child(Token) ->
  supervisor:start_child(?MODULE, [Token]).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => simple_one_for_one
              , intensity => 0
              , period    => 1
              },
  ChildSpecs = [#{ id       => els_progress_server
                 , start    => {els_progress_server, start_link, []}
                 , shutdown => brutal_kill
                 }],
  {ok, {SupFlags, ChildSpecs}}.
