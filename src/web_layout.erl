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
-export([register_header/3, register_footer/3]).

-include("logger.hrl").

-record(state, {}).

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
%% Function: load_pages(WebPagesName, Directory) -> ok | {error,Error}
%% Description: Loads pages in directory, compiles them and registers
%%               routes to them.
%%--------------------------------------------------------------------
register_header(Prefix, WebRouter, File) when is_atom(Prefix) ->
  gen_server:cast(?SERVER, {register_header, atom_to_list(Prefix), WebRouter, File});
register_header(Prefix, WebRouter, File) ->
  gen_server:cast(?SERVER, {register_header, Prefix, WebRouter, File}).

register_footer(Prefix, WebRouter, File) when is_atom(Prefix) ->
  gen_server:cast(?SERVER, {register_footer, atom_to_list(Prefix), WebRouter, File});
register_footer(Prefix, WebRouter, File) ->
  gen_server:cast(?SERVER, {register_footer, Prefix, WebRouter, File}).

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
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_header, Prefix, WebRouter, File}, State) ->
  ?DEBUG("Loading up ~p for router ~p with prefix ~p~n~n", [File, WebRouter, Prefix]),
  OutDir = case code:which(?MODULE) of
             non_existing ->
               filename:dirname(File);
             preloaded ->
               filename:dirname(File);
             cover_compiled ->
               filename:dirname(File);
             ModulePath ->
               filename:dirname(ModulePath)
           end,
  StrippedFileName = filename:rootname(filename:basename(File)),
  ModuleName = Prefix ++ "_" ++ StrippedFileName,
  case haml:compile(File, [{module, ModuleName},
                           {outdir, OutDir}, report_errors,
                           report_warnings, nowarn_unused_vars]) of
    ok ->
      web_router:add(WebRouter, pre_request_view, global,
                     list_to_atom(ModuleName), render, 1);
    {error, Reason} ->
      ?ERROR_MSG("Haml Compile failed for ~s with: ~p~n",
                 [File, Reason])
  end,
  {noreply, State};
handle_cast({register_footer, Prefix, WebRouter, File}, State) ->
  ?DEBUG("Loading up ~p for router ~p with prefix ~p~n~n", [File, WebRouter, Prefix]),
  OutDir = case code:which(?MODULE) of
             non_existing ->
               filename:dirname(File);
             preloaded ->
               filename:dirname(File);
             cover_compiled ->
               filename:dirname(File);
             ModulePath ->
               filename:dirname(ModulePath)
           end,
  StrippedFileName = filename:rootname(filename:basename(File)),
  ModuleName = Prefix ++ "_" ++ StrippedFileName,
  case haml:compile(File, [{module, ModuleName},
                           {outdir, OutDir}, report_errors,
                           report_warnings, nowarn_unused_vars]) of
    ok ->
      web_router:add(WebRouter, post_request_view, global,
                     list_to_atom(ModuleName), render, 1);
    {error, Reason} ->
      ?ERROR_MSG("Haml Compile failed for ~s with: ~p~n",
                 [File, Reason])
  end,
  {noreply, State};

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
