%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Апр. 2016 16:49
%%%-------------------------------------------------------------------
-module(btcd_wallet_test).
-author("anoskov").

-include_lib("eunit/include/eunit.hrl").

start_service_test() ->
  Res = btcd_wallet:start_link(),
  ?assertMatch({ok, _Pid}, Res).

get_balance_test() ->
  Pid = whereis(btcd_wallet),
  ?assertMatch({ok, Balance}, btcd_wallet:getbalance(Pid)).