-module(my_esi).
-export([foo/3,bar/3]).

foo(Sid, Env, In) -> mod_esi:deliver(Sid, ["foo"]).
bar(Sid, Env, In) -> mod_esi:deliver(Sid, ["bar"]).
