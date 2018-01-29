-module(jesse_endpoints_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

definitions_test() ->
    JEP = endpoints:prepare_validation(),
    {_Good, Bad} = lists:splitwith(fun(X) -> X == ok end, JEP),
    ?assertEqual([], Bad),
    ?assertEqual({ok, lists:seq(1,42)}, 
                 jesse:validate("/definitions/Pow", 
                                [ I || I<-lists:seq(1,42) ])),
    {error, [{data_invalid, _, wrong_type, _, _}]} = 
        jesse:validate("/definitions/Pow", [ "bla" || _ <- lists:seq(1,4) ]),
    {error, [{data_invalid, _, wrong_size, _, _}]} = 
        jesse:validate("/definitions/Pow", [ I || I <- lists:seq(1,4) ]).

%% This tests reveals uncovered cases in creation json_schema from swagger
%% The difference are in "error" versus error tags
json_schema_test() ->
  Map = endpoints:json_schema(),
  Schema = jsx:prettify(jsx:encode(Map)),
  ?assertEqual(Map, jsx:decode(Schema, [return_maps])).


-endif.
