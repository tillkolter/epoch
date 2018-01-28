-module(jesse_endpoints).

-export([load_definitions/0]).

load_definitions() ->
    [ jesse:add_schema(Def, Schema) || {Def, Schema} <- endpoints:definitions() ].

