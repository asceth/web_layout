%%%-------------------------------------------------------------------
%%% File    : web_layout.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Loads up a given directory's view files, compiles
%%%                them and registers routes to them.
%%%
%%% Created : 20 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_layout).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External API
-export([layout_view/1, register_layout/3, yield/1]).

-include("logger.hrl").

-record(state, {layouts=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: register_layout(Name, WebRouter, File) -> ok | {error,Error}
%% Description: Registers a layout in herml with specified name to a web router.
%%--------------------------------------------------------------------
register_layout(Name, WebRouter, File) ->
  gen_server:cast(?SERVER, {register_layout, Name, WebRouter, File}).

%%--------------------------------------------------------------------
%% Function: header_view(Session) -> iolist()
%% Description: Renders the appropriate layout header.  'default' is the default layout.
%%--------------------------------------------------------------------
layout_view(Session) ->
  gen_server:call(?SERVER, {execute_layout, Session}).

%%--------------------------------------------------------------------
%% Function: yield(proplist()) -> string | binary
%% Description: looks up YieldedContent from the property list and displays it
%%--------------------------------------------------------------------
yield(Env) ->
  case lists:keysearch("Session", 1, Env) of
    false ->
      <<"">>;
    {value, {_Key, Value}} ->
      case Value:flash_lookup("YieldedContent") of
        {error, 404} ->
          <<"">>;
        YieldedContent ->
          YieldedContent
      end
  end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({execute_layout, Session}, From, #state{layouts=Layouts} = State) ->
  spawn(fun() -> execute_layout(From, web_session:flash_lookup(Session, "layout"), Layouts, Session) end),
  {noreply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_layout, Name, WebRouter, File}, State) ->
  ?DEBUG("Loading up ~p for router ~p for layout ~p~n~n", [File, WebRouter, Name]),
  NewLayout = case herml_parser:file(File) of
                {error, Reason} ->
                  ?ERROR_MSG("Herml Compile failed for ~s with: ~p~n", [File, Reason]),
                  [];
                CompiledTemplate ->
                  web_router:add(WebRouter, request_layout_view, global, web_layout, layout_view, 1),
                  [{Name, CompiledTemplate}]
              end,
  Layouts = State#state.layouts,
  State1 = #state{layouts=Layouts ++ NewLayout},
  {noreply, State1};
handle_cast(_Msg, State) ->
  ?ERROR_MSG("Did not recognize: ~p", [_Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

execute_layout(From, {error, 404}, Layouts, Session) ->
  Reply = case lists:keysearch(default, 1, Layouts) of
            false ->
              % if no layout try to render just the YieldedContent from the session
              case web_session:flash_lookup(Session, "YieldedContent") of
                {error, 404} ->
                  <<"">>;
                YieldedContent ->
                  YieldedContent
              end;
            {value, {_Key, Value}} ->
              herml_htmlizer:render(Value, [{"Session", Session}])
          end,
  gen_server:reply(From, Reply);

execute_layout(From, Layout, Layouts, Session) ->
  Reply = case lists:keysearch(Layout, 1, Layouts) of
            false ->
              execute_layout(From, {error, 404}, Layouts, Session);
            {value, {_Key, Value}} ->
              herml_htmlizer:render(Value, [{"Session", Session}])
          end,
  gen_server:reply(From, Reply).

