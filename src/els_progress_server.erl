%%==============================================================================
%% The Erlang Progress Report Server
%%==============================================================================
-module(els_progress_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ start_link/1
        , work_done_progress_begin/4
        , work_done_progress_report/3
        , work_done_progress_end/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{ token := token() }.

-type token()      :: binary().
-type title()      :: binary().
-type msg()        :: binary().
-type percentage() :: 0..100.
-type kind()       :: binary().
-type value()      :: #{ kind        := kind()
                       , title       => title()
                       , cancellable => boolean()
                       , message     := msg()
                       , percentage  => percentage()
                       }.

-export_type([ token/0 ]).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(token()) -> {ok, pid()}.
start_link(Token) ->
  gen_server:start_link(?MODULE, Token, []).

%% TODO: Rename msg into message once naming conflict is solved
-spec work_done_progress_begin(pid(), title(), msg(), percentage()) -> ok.
work_done_progress_begin(Server, Title, Message, Percentage) ->
  Request = {work_done_progress_begin, Title, Message, Percentage},
  gen_server:cast(Server, Request).

-spec work_done_progress_report(pid(), msg(), percentage()) -> ok.
work_done_progress_report(Server, Message, Percentage) ->
  Request = {work_done_progress_report, Message, Percentage},
  gen_server:cast(Server, Request).

-spec work_done_progress_end(pid(), msg()) -> ok.
work_done_progress_end(Server, Message) ->
  Request = {work_done_progress_end, Message},
  gen_server:cast(Server, Request).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(token()) -> {ok, state()}.
init(Token) ->
  {ok, #{ token => Token }}.

-spec handle_call(any(), any(), state()) -> {noreply, state()}.
handle_call(_Message, _From, State) ->
  {noreply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({work_done_progress_begin, Title, Message, Percentage}, State) ->
  #{ token := Token } = State,
  Value = #{ kind        => <<"begin">>
           , title       => Title
           , cancellable => false
           , message     => Message
           , percentage  => Percentage
           },
  send_progress_notification(Token, Value),
  {noreply, State};
handle_cast({work_done_progress_report, Message, Percentage}, State) ->
  #{ token := Token } = State,
  Value = #{ kind        => <<"report">>
           , cancellable => false
           , message     => Message
           , percentage  => Percentage
           },
  send_progress_notification(Token, Value),
  {noreply, State};
handle_cast({work_done_progress_end, Message}, State) ->
  #{ token := Token } = State,
  Value = #{ kind        => <<"end">>
           , message     => Message
           },
  send_progress_notification(Token, Value),
  {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec send_progress_notification(token(), value()) -> ok.
send_progress_notification(Token, Value) ->
  Method = <<"$/progress">>,
  Params = #{ token => Token
            , value => Value
            },
  els_server:send_notification(Method, Params).
