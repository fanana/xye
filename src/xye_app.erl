%% Copyright 2020, XYE <xye.proto@protonmail.ch>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 3 of the License, or (at your option) any later version.

-module(xye_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    xye_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
