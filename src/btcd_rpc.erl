%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Апр. 2016 17:02
%%%-------------------------------------------------------------------
-module(btcd_rpc).
-author("anoskov").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 30000).
-define(SEED_BYTES, 16).
-define(HTTP_REQUEST_TIMEOUT,    30000).
-define(HTTP_CONNECTION_TIMEOUT, 3000).

-record(btcd_conf, {
  user     = <<"">>          :: binary(),
  password = <<"">>          :: binary(),
  host     = <<"127.0.0.1">> :: binary(),
  port     = 8332            :: pos_integer(),
  ssl      = false           :: boolean()
}).

-type btcd_conf() :: #btcd_conf{}.

-record(state, {
  config :: btcd_conf(),
  seed = crypto:strong_rand_bytes(?SEED_BYTES) :: binary()
}).

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
  Config = #btcd_conf{
    user     = os:getenv("RPC_USER"),
    password = os:getenv("RPC_PASSWORD")
  },
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

-spec(start_link(binary()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Url) when is_binary(Url) ->
  Config = rpc_url(unicode:characters_to_list(Url)),
  gen_server:start_link({local, ?SERVER}, [Config], []).

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
init([Config = #btcd_conf{}]) ->
  {ok, #state{config = Config}}.

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
handle_call(Request, _From, State)
  when is_atom(Request) ->
  handle_rpc(Request, [], State);
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
%%% Internal functions
%%%===================================================================

-spec(handle_rpc(Request :: term(), Params :: term(),
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}}).
handle_rpc( Request, Params, State = #state{seed = Seed, config = Config}) ->
  Method = erlang:atom_to_binary(Request, utf8),
  JsonReq = jsonrpc2_client:create_request(
    {Method, mochijson2:decode(jsxn:encode(Params)), seed_to_utf8(Seed)}
  ),
  {
    reply,
    rpc(JsonReq, Config),
    State#state{seed = increment_seed(Seed)}
  }.

seed_to_utf8(Seed) when is_binary(Seed) ->
  base64:encode(Seed).

increment_seed(Bin) when is_binary(Bin) ->
  crypto:strong_rand_bytes(?SEED_BYTES).

rpc(JsonReq, Config) ->
  Request = build_rpc_req(JsonReq, Config),
  HTTPOptions = [
    {timeout,         ?HTTP_REQUEST_TIMEOUT   },
    {connect_timeout, ?HTTP_CONNECTION_TIMEOUT},
    {autoredirect,    true                    }
  ],
  Options = [],

  {ok, _Response = {
    {_HTTPVersion, StatusCode, _StatusText},
    _RespHeaders, RespBody}
  } = httpc:request(post, Request, HTTPOptions, Options),

  RespDecoded = jsxn:decode(unicode:characters_to_binary(RespBody)),
  case StatusCode of
    OkStatus when (OkStatus >= 200) and (OkStatus =< 299)->
      {ok, maps:get(<<"result">>, RespDecoded, RespDecoded)};
    _ ->
      {error, maps:get(<<"error">>, RespDecoded, RespDecoded)}
  end.

build_rpc_req(JsonReq, Config) ->
  Url = unicode:characters_to_list(rpc_url(Config)),
  Body = iolist_to_binary(mochijson2:encode(JsonReq)),
  ContentType = "application/json",
  C = fun unicode:characters_to_list/1,
  Headers = [
    {"Authorization",
        "Basic " ++ base64:encode_to_string(C(Config#btcd_conf.user) ++ ":" ++ C(Config#btcd_conf.password))
    }
  ],
  {Url, Headers, ContentType, Body}.

rpc_url(#btcd_conf{user = _User, password = _Pass, host = Host, port = Port, ssl = Ssl}) ->
  <<
    (case Ssl of
       true -> <<"https">>;
       false -> <<"http">>
     end)/binary, <<"://">>/binary,
    Host/binary, <<":">>/binary,
    (erlang:integer_to_binary(Port))/binary,
    <<"/">>/binary
  >>.