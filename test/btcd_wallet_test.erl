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

start_rpc_service_test() ->
  Res = btcd_rpc:start_link(),
  ?assertMatch({ok, _Pid}, Res).

start_wallet_service_test() ->
  Res = btcd_wallet:start_link(),
  ?assertMatch({ok, _Pid}, Res).

get_balance_test() ->
  Pid = whereis(btcd_rpc),
  ?assertMatch({ok, _Balance}, btcd_wallet:getbalance(Pid)).

get_balance_by_account_test() ->
  Pid = whereis(btcd_rpc),
  ?assertMatch({ok, _Balance}, btcd_wallet:getbalance(Pid, <<"13Lnq2r8YG5nqdfy35gCcpYf16msumrJve">>)).

get_account_address_test() ->
  Pid = whereis(btcd_rpc),
  ?assertMatch({ok, _Address}, btcd_wallet:getaccountaddress(Pid, <<"13Lnq2r8YG5nqdfy35gCcpYf16msumrJve">>)).

get_wallet_info_test() ->
  Pid = whereis(btcd_rpc),
  ?assertMatch({ok, _Info}, btcd_wallet:getwalletinfo(Pid)).

get_new_address_test() ->
  Pid = whereis(btcd_rpc),
  ?assertMatch({ok, _Address}, btcd_wallet:getnewaddress(Pid)).