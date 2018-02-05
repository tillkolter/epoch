%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module dealing with Blake2s Proof of Work calculation and hash generation
%%% @end
%%%=============================================================================
-module(aec_pow_blake2s).

-behaviour(aec_pow).

-export([generate/3,
         verify/4]).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("pow.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Return whether the specified nonce value, when used in hash
%% calculation of 'Data', results in a smaller value than the target.
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_hash:hashable(), Target :: aec_pow:sci_int(),
               Nonce :: aec_pow:nonce()) -> aec_pow:pow_result().
generate(Data, Target, Nonce) when Nonce >= 0,
                                   Nonce =< ?MAX_NONCE ->
    Hash1 = aec_hash:blake2s_hash(Data),
    Hash2 = aec_hash:blake2s_hash(<<Hash1/binary, Target:16, Nonce:?NONCE_BITS>>),
    case aec_pow:test_target(Hash2, Target) of
        true ->
            %% Hash satisfies condition: return nonce
            {ok, {Nonce, no_value}};
        false ->
            {error, no_solution}
    end.

%%------------------------------------------------------------------------------
%% Proof of Work verification (with target check)
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_hash:hashable(), Nonce :: aec_pow:nonce(),
             Evd :: aec_pow:pow_evidence(), Target :: aec_pow:sci_int()) ->
                    boolean().
verify(Data, Nonce, Evd, Target) when Evd == no_value,
                                      Nonce >= 0,
                                      Nonce =< ?MAX_NONCE ->
    %% Verification: just try if current Nonce satisfies target threshold
    case generate(Data, Target, Nonce) of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
