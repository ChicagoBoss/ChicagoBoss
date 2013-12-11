-module(make_app_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([make_app/1]).

all() -> [make_app].

%% This would be a make_app/1 not a make_app/0
make_app(_Config) -> 
    os:cmd("cd ../../ && make app PROJECT=testproj").

