%% Copyright 2020, XYE <xye.proto@protonmail.ch>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 3 of the License, or (at your option) any later version.

-module(xye).

-behaviour(gen_statem).

%% API
-export([start_link/0, start_link/1,
	 cap1/1, cap2/1, oper/1, fan/1, temp/1,
	 t1/1, t2a/1, t2b/1,
	 t3/1, current/1, ff_03/1,
	 on/1, off/1,
	 run/1, mode/1, water/1,
	 protect/1, error/1, ccm/1,
	 test/1, lock/1,
	 format/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-include("xye.hrl").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10).

-record(data, {tty, ac, ts, last}).

%%===================================================================
%% API
%%===================================================================

start_link() ->
    {ok, TTY} = application:get_env(port),
    start_link(TTY).

start_link(TTY) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [TTY], []).

oper(Oper)
  when Oper =:= off;
       Oper =:= auto;
       Oper =:= cool;
       Oper =:= dry;
       Oper =:= heat;
       Oper =:= fanauto;
       is_integer(Oper) ->
    gen_statem:call(?SERVER, {?FUNCTION_NAME, 2, Oper}).

fan(Mode)
  when Mode =:= auto;
       Mode =:= high;
       Mode =:= medium;
       Mode =:= low;
       is_integer(Mode) ->
    gen_statem:call(?SERVER, {?FUNCTION_NAME, 2, Mode}).

temp(Int) ->
    set_int(?FUNCTION_NAME, Int).

cap1(Int) ->
    set_int(?FUNCTION_NAME, Int).

cap2(Int) ->
    set_int(?FUNCTION_NAME, Int).

t1(Int) ->
    set_int(?FUNCTION_NAME, Int).

t2a(Int) ->
    set_int(?FUNCTION_NAME, Int).

t2b(Int) ->
    set_int(?FUNCTION_NAME, Int).

t3(Int) ->
    set_int(?FUNCTION_NAME, Int).

current(Int) ->
    set_int(?FUNCTION_NAME, Int).

ff_03(Int) ->
    set_int(?FUNCTION_NAME, Int).

run(Int) ->
    set_int(?FUNCTION_NAME, Int).

mode(Mode)
  when Mode =:= normal;
       Mode =:= turbo;
       Mode =:= eco;
       Mode =:= swing;
       Mode =:= vent;
       is_integer(Mode) ->
    gen_statem:call(?SERVER, {?FUNCTION_NAME, 2, Mode}).

water(Int) ->
    set_int(?FUNCTION_NAME, Int).

protect(Int) ->
    set_int(?FUNCTION_NAME, Int).

error(Int) ->
    set_int(?FUNCTION_NAME, Int).

ccm(Int) ->
    set_int(?FUNCTION_NAME, Int).

on(Int) ->
    set_int(?FUNCTION_NAME, Int).

off(Int) ->
    set_int(?FUNCTION_NAME, Int).

lock(Lock)
  when Lock =:= locked;
       Lock =:= unlocked;
       is_integer(Lock) ->
    gen_statem:call(?SERVER, {?FUNCTION_NAME, 2, Lock}).

test(Int) ->
    set_int(?FUNCTION_NAME, Int).

set_int(What, Int) when is_integer(Int) ->
    gen_statem:call(?SERVER, {What, 2, Int}).

format() ->
    gen_statem:call(?SERVER, {format, 1}).

%%===================================================================
%% gen_server callbacks
%%===================================================================

callback_mode() -> [handle_event_function, state_enter].

init([TTY]) ->
    process_flag(trap_exit, true),

    F = serial:start([{open, TTY}, {speed, 4800}]),
    AC = #{2 => #xye{id = 2, monitor = false}},
    Now = erlang:monotonic_time(microsecond),
    Data = #data{
	      tty = F,
	      ac = AC,
	      ts = Now,
	      last = Now
	      },
    {ok, <<>>, Data}.

handle_event(enter, _, <<>>, _Data) ->
    keep_state_and_data;
handle_event(enter, _, _, _Data) ->
    {keep_state_and_data, [{state_timeout, ?TIMEOUT, recv}]};

handle_event(info, {data, _SerialPid, Bytes}, <<>>, Data) ->
    Now = erlang:monotonic_time(microsecond),
    {next_state, Bytes, Data#data{ts = Now}};
handle_event(info, {data, Bytes}, Buffer, Data) ->
    {next_state, <<Buffer/binary, Bytes/binary>>, Data};
handle_event(state_timeout, recv, Buffer, #data{ts = TS} = Data0) ->
    Data = process(Buffer, Data0),
    {next_state, <<>>, Data#data{last = TS}};

handle_event({call, From}, {What, Id, Value}, State, #data{ac = AC0} = Data) ->
    AC = maps:update_with(Id, fun(V) -> set(What, Value, V),set(What, Value, V) end,
			  set(What, Value, #xye{id = Id}), AC0),
    {next_state, State, Data#data{ac = AC}, [{reply, From, ok}]};

handle_event({call, From}, {format, Id}, _State, #data{ac = AC})
  when is_map_key(Id, AC) ->
    R = format_ac(maps:get(Id, AC), ""),
    {keep_state_and_data, [{reply, From, R}]};

handle_event(Type, Content, _State, _Data) ->
    io:format("unexpected event: ~p:~p~n", [Type, Content]),
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

hex(Value) when is_binary(Value) ->
    erlang:iolist_to_binary([io_lib:format(" ~2.16.0b", [X]) || <<X>> <= Value]).

process(B, Data) ->
    Len = size(B),
    case B of
	<<16#AA, PayLoad:(Len - 3)/bytes, _CRC:8, 16#55>> = B ->
	    process(PayLoad, make_crc(B), Data);
	_ ->
	    io:format("F:~s\n", [hex(B)]),
	    Data
    end.

%% Query
process(<<16#C0, Id:8, Src:8, 16#80, Dst:8,
	  0, 0, 0, 0, 0, 0, 0, 16#3f>> = B, 0, #data{tty = F, ac = AC} = Data)
  when is_map_key(Id, AC) ->
    io:format("~10w Q:~s                                                          ~n",
	      [tdiff(Data), hex(B)]),
    query_state(F, 0, Id, Src, Dst, maps:get(Id, AC)),
    Data;
%% Set
process(<<16#C3, Id:8, Src:8, 16#80, Dst:8, Set:7/bytes, 16#3c>> = B,
	0, #data{tty = F, ac = ACs} = Data)
  when is_map_key(Id, ACs) ->
    io:format("~10w S:~s                                                          ~n", [tdiff(Data), hex(B)]),
    AC = set_state(F, Id, Src, Dst, Set, maps:get(Id, ACs)),
    Data#data{ac = maps:put(Id, AC, ACs)};

process(<<16#CC, Id:8, Src:8, 16#80, Dst:8, _:7/bytes, 16#33>> = B,
	0, #data{tty = F, ac = ACs} = Data)
  when is_map_key(Id, ACs) ->
    io:format("~10w S:~s                                                      LOCK~n", [tdiff(Data), hex(B)]),
    AC0 = maps:get(Id, ACs),
    AC = AC0#xye{lock = locked},
    query_state(F, 16#0C, Id, Src, Dst, AC),
    Data#data{ac = maps:put(Id, AC, ACs)};

process(<<16#CD, Id:8, Src:8, 16#80, Dst:8, _:7/bytes, 16#32>> = B,
	0, #data{tty = F, ac = ACs} = Data)
  when is_map_key(Id, ACs) ->
    io:format("~10w S:~s                                                    UNLOCK~n", [tdiff(Data), hex(B)]),
    AC0 = maps:get(Id, ACs),
    AC = AC0#xye{lock = unlocked},
    query_state(F, 16#0D, Id, Src, Dst, AC),
    Data#data{ac = maps:put(Id, AC, ACs)};

process(<<_:8, 16#800e:16, _/binary>> = B, _, Data) ->
    io:format("~10w a:~s                                                          ~n", [tdiff(Data), hex(B)]),
    Data;
process(<<16#C0, 16#80, Dst:8, Id:8, Src:8, Report/binary>> = B, 0, #data{ac = ACs} = Data) ->
    AC = monitor_state(Id, Src, Dst, Report, maps:get(Id, ACs, #xye{id = Id, monitor = true})),
    io:format("~10w M:~s\n", [tdiff(Data), hex(B)]),
    io:format("S: ~s", [format_ac(AC, "   ")]),
    Data#data{ac = maps:put(Id, AC, ACs)};
process(<<16#C0, Id:8, _/binary>>, _CRC, Data) ->
    io:format("~10w -:~2.16.0b\n", [tdiff(Data), Id]),
    Data;
process(B, _, Data) ->
    io:format("~10w -:~s\n", [tdiff(Data), hex(B)]),
    Data.

tdiff(#data{ts = TS, last = Last}) ->
    TS - Last.

query_state(F, Code, Id, Src, Dst, #xye{monitor = false} = AC) ->
    State = serialize_state(AC),
    A = make_answer(Code, Id, Src, Dst, State, 0),
    CRC = make_crc(A),
    B = make_answer(Code, Id, Src, Dst, State, CRC),
    io:format("           A:~s                                                          ~n", [hex(B)]),
    F ! {send, B},
    ok;
query_state(_F, _Code, _Id, _Src, _Dst, _AC) ->
    ok.

set_state(F, Id, Src, Dst, <<Oper, Fan, Temp, Mode, On, Off, 0>>,
	  #xye{monitor = false} = AC0) ->
    AC = AC0#xye{
	   oper = dec_ac_oper(Oper),
	   fan = dec_ac_fan(Fan),
	   temp = Temp,
	   on = On,
	   off = Off,
	   mode = dec_ac_mode(Mode)
	  },
    %% send_ack --- TODO: check!!
    io:format("~s", [format_ac(AC, "")]),
    query_state(F, 3, Id, Src, Dst, AC),
    AC;
set_state(_F, _Id, _Src, _Dst, _State, AC) ->
    AC.

%% A: aa c0 80 0e 01 0e 30 10 88 80 14 5f 4c ff 64 ff ff 00 00 01 00 04 00 00 00 00 00 00 00 00 36 55
%%%A: aa c0 80 0e 01 0e 30 10 80 80 14 60 59 ff 59 ff ff 00 00 01 00 04 00 00 00 00 00 00 00 00 3b 55

monitor_state(_Id, _Src, _Dst,
	      <<Cap1:8, Cap2:8,    Oper:8,  Fan:8,
		Temp:8, T1:8,      T2A:8,   T2B:8,
		T3:8,   Current:8, FF_03:8, On:8,
		Off:8,  Run:8,     Mode:8,  Water:8,
		Error:16/little,   Protect:16/little,
		CCM:8,  L1:8,      L2:8,    L3:8>>, AC) ->
    Lock = if (Water bor 16#80) =/= 0 -> locked;
	      true                    -> unlocked
	   end,
    AC#xye{
      cap1 = Cap1,
      cap2 = Cap2,
      oper = dec_ac_oper(Oper),
      fan = dec_ac_fan(Fan),
      temp = Temp,
      t1 = T1,
      t2a = T2A,
      t2b = T2B,
      t3 = T3,
      current = Current,
      ff_03 = FF_03,
      on = On,
      off = Off,
      run = Run,
      mode = dec_ac_mode(Mode),
      water = Water band 16#7f,
      error = Error,
      protect = Protect,
      ccm = CCM,
      l1 = L1,
      l2 = L2,
      l3 = L3,
      lock = Lock
     }.

serialize_state(AC) ->
    Lock = case AC#xye.lock of
	       locked -> 16#80;
	       _      -> 16#00
	   end,
io:format("ater: ~p, Lock: ~p, ~p~n", [AC#xye.water, AC#xye.lock, Lock]),
    <<(AC#xye.cap1),
      (AC#xye.cap2),
      (enc_ac_oper(AC)),
      (enc_ac_fan(AC)),
      (AC#xye.temp),
      (AC#xye.t1),
      (AC#xye.t2a),
      (AC#xye.t2b),
      (AC#xye.t3),
      (AC#xye.current),
      (AC#xye.ff_03),
      (AC#xye.on),
      (AC#xye.off),
      (AC#xye.run),
      (enc_ac_mode(AC)),
      (AC#xye.water bor Lock),
      (AC#xye.error):16/little,
      (AC#xye.protect):16/little,
      (AC#xye.ccm),
      (AC#xye.l1),
      (AC#xye.l2),
      (AC#xye.l3)>>.

enc_ac_oper(#xye{oper = cool}) -> 16#88;
enc_ac_oper(#xye{oper = heat}) -> 16#84;
enc_ac_oper(#xye{oper = dry})  -> 16#82;
enc_ac_oper(#xye{oper = fan})  -> 16#81;
enc_ac_oper(#xye{oper = auto}) -> 16#80;
enc_ac_oper(#xye{oper = off})  -> 16#00;
enc_ac_oper(#xye{oper = I}) when is_integer(I) -> I;
enc_ac_oper(#xye{oper = _})    -> 16#00.

dec_ac_oper(16#88) -> cool;
dec_ac_oper(16#84) -> heat;
dec_ac_oper(16#82) -> dry;
dec_ac_oper(16#81) -> fan;
dec_ac_oper(16#80) -> auto;
dec_ac_oper(16#00) -> off;
dec_ac_oper(I) when is_integer(I) -> I.

enc_ac_fan(#xye{fan = high})   -> 1;
enc_ac_fan(#xye{fan = medium}) -> 2;
enc_ac_fan(#xye{fan = low})    -> 4;
enc_ac_fan(#xye{fan = auto})  -> 16#80;
enc_ac_fan(#xye{fan = I}) when is_integer(I) -> I;
enc_ac_fan(#xye{fan = _})     -> 16#80.

dec_ac_fan(1) -> high;
dec_ac_fan(2) -> medium;
dec_ac_fan(4) -> low;
dec_ac_fan(16#80) -> auto;
dec_ac_fan(I) when is_integer(I) -> I.

enc_ac_mode(#xye{mode = normal}) -> 16#00;
enc_ac_mode(#xye{mode = eco})    -> 16#01;
enc_ac_mode(#xye{mode = turbo})  -> 16#02;
enc_ac_mode(#xye{mode = swing})  -> 16#04;
enc_ac_mode(#xye{mode = vent})   -> 16#88;
enc_ac_mode(#xye{mode = I}) when is_integer(I) -> I;
enc_ac_mode(#xye{mode = _})      -> 0.

dec_ac_mode(16#00) -> normal;
dec_ac_mode(16#01) -> eco;
dec_ac_mode(16#02) -> turbo;
dec_ac_mode(16#04) -> swing;
dec_ac_mode(16#88) -> vent;
dec_ac_mode(I) when is_integer(I) -> I.

make_crc(Bin) ->
    255 - (lists:foldl(fun(X, Sum) -> Sum + X end, 0, binary_to_list(Bin)) rem 256).

make_answer(Code, Id, Src, Dst, State, CRC) ->
    <<16#AA, (16#C0 bor Code), 16#80, Dst, Id, Src, State/binary, CRC, 16#55>>.

set(cap1, Mode, AC) ->     AC#xye{cap1 = Mode};
set(cap2, Mode, AC) ->     AC#xye{cap2 = Mode};
set(oper, Oper, AC) -> AC#xye{oper = Oper};
set(fan, Mode, AC) ->      AC#xye{fan = Mode};
set(temp, Int, AC) ->      AC#xye{temp = Int};
set(t1, Int, AC) ->        AC#xye{t1 = Int};
set(t2a, Int, AC) ->       AC#xye{t2a = Int};
set(t2b, Int, AC) ->       AC#xye{t2b = Int};
set(t3, Int, AC) ->        AC#xye{t3 = Int};
set(current, Int, AC) ->   AC#xye{current = Int};
set(ff_03, Int, AC) ->     AC#xye{ff_03 = Int};
set(on, Int, AC) ->        AC#xye{on = Int};
set(off, Int, AC) ->       AC#xye{off = Int};
set(run, Int, AC) ->       AC#xye{run = Int};
set(mode, Mode, AC) ->     AC#xye{mode = Mode};
set(water, Int, AC) ->     AC#xye{water = Int};
set(error, Int, AC) ->     AC#xye{error = Int};
set(protect, Int, AC) ->   AC#xye{protect = Int};
set(ccm, Int, AC) ->       AC#xye{ccm = Int};
set(lock, Lock, AC) ->     AC#xye{lock = Lock};
set(l1, Int, AC) ->        AC#xye{l1 = Int};
set(l2, Int, AC) ->        AC#xye{l2 = Int};
set(l3, Int, AC) ->        AC#xye{l3 = Int}.

format_ac(#xye{id = Id, cap1 = Cap1, cap2 = Cap2,
		 oper = Oper, fan = Fan,
		 temp = Temp, t1 = T1, t2a = T2A, t2b = T2B,
		 t3 = T3, current = Current, ff_03 = FF_03,
		 on = On, off = Off, run = Run,
		 mode = Mode, water = Water,
		 protect = Protect, error = Error, ccm = CCM,
		 lock = Lock, l1 = L1, l2 = L2, l3 = L3}, Indent) ->
    lists:flatten(
      io_lib:format(
	"Id: ~2w, Cap: ~2.16.0b ~2.16.0b, "
	"OperMode ~w, Lock: ~w, Fan ~w, Temp ~w °C, T1 ~f °C, "
	"T2A ~f °C, T2B  ~f °C, T3 ~f °C, Current ~w A, FF_03 ~w~n"
	"~s        On ~f h, Off ~f h, "
	"Run ~w, Mode ~w, Water ~w, Protect ~w, Error ~w, CCM ~w, "
	"L ~2.16.0b ~2.16.0b ~2.16.0b~n",
	[Id, Cap1, Cap2, Oper, Lock, Fan, Temp, scale_temp(T1), scale_temp(T2A),
	 scale_temp(T2B), scale_temp(T3), Current,
	 FF_03, Indent,
	 fmt_timer(On), fmt_timer(Off),
	 Run, Mode, Water, Protect, Error, CCM,
	 L1, L2, L3])).

scale_temp(T)
  when T < 40; T > 238 ->
    0.0;
scale_temp(T) ->
    (T - 40) / 2.

fmt_timer(Timer) ->
    fmt_timer(Timer bsr 1, Timer rem 2, 1, 0.0).

fmt_timer(_, _, 256, Sum) ->
    Sum;
fmt_timer(Next, IsSet, Exp, Sum) ->
    fmt_timer(Next bsr 1, Next rem 2, Exp bsl 1, Sum + 0.25 * Exp * IsSet).
