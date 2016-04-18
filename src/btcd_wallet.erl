%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Апр. 2016 15:48
%%%-------------------------------------------------------------------
-module(btcd_wallet).
-author("anoskov").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Service Functions
-export([getbalance/1,
  getbalance/2,
  getaccountaddress/2,
  getwalletinfo/1,
  getnewaddress/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  walletlock/1]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 30000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Service Functions
%%%===================================================================

-spec(getbalance(pid() ) -> {ok, float()} | {error, term()}).
getbalance(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, getbalance, ?TIMEOUT).

-spec(getbalance(pid(), binary()) -> {ok, float()} | {error, term()}).
getbalance(Pid, Account) when is_pid(Pid), is_binary(Account) ->
  gen_server:call( Pid, {getbalance, [Account]}, ?TIMEOUT).

-spec(getaccountaddress(pid(), binary()) -> {ok, binary()} | {error, term()}).
getaccountaddress(Pid, Account) when is_pid(Pid), is_binary(Account) ->
  gen_server:call(Pid, {getaccountaddress, [Account]}, ?TIMEOUT).

-spec(getwalletinfo(pid()) -> {ok, binary()} | {error, term()}).
getwalletinfo(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, getwalletinfo, ?TIMEOUT).

-spec(getnewaddress(pid()) -> {ok, binary()} | {error, term()}).
getnewaddress(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, getnewaddress, ?TIMEOUT).

-spec(walletlock(pid()) -> {ok, term()} | {error, term()}).
walletlock(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, walletlock, ?TIMEOUT).

%%%===================================================================
%%% Internal Functions
%%%===================================================================