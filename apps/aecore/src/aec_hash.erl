-module(aec_hash).

-export([hash/2,
         blake2s_hash/1]).

-export_type([hashable/0, hash/0]).

-type hashable() :: binary().

-type hash() :: binary().

-spec hash(atom(), hashable()) -> hash().
hash(pow, Bin) when is_binary(Bin) ->
    aec_sha256:hash(Bin);
hash(_ObjType, Bin) when is_binary(Bin) ->
    blake2s_hash(Bin).

blake2s_hash(Bin) ->
    {ok, Hash} = enacl:generichash(32, Bin),
    Hash.
