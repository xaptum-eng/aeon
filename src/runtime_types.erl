
-module(runtime_types).

-export([parse_transform/2, do_transform/2]).

-include_lib("parse_trans/include/codegen.hrl").

-define(FUN_NAME, '#types').

%%-define(dbg(DbgMsg, Args), io:format(DbgMsg, Args)).
-define(dbg(DbgMsg, Args), noop).

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    F = erl_syntax_lib:analyze_forms(Forms),
    ?dbg("Forms = ~p~n", [Forms]),
    ?dbg("F = ~p~n", [F]),
    case {lists:keyfind(records, 1, F), lists:keyfind(attributes, 1, F)} of
	{false, false} -> 
	    ?dbg("No keys: records, type~n", []),
	    Forms;
	{{records, R}, false} ->
	    Attr = lists:flatten([transform_attribute_rec(Rec) || Rec <- R]), 
	    do_transform_2(Attr, Forms, Context);
	{false, {type, Rt}} ->
	    Attr = lists:flatten([transform_attribute(Rtype) || Rtype <- Rt]), 
	    do_transform_2(Attr, Forms, Context);
	{{records, Rr}, {attributes, Rt}} ->
	    AttrRec = lists:flatten([transform_attribute_rec(Rec) || Rec <- Rr]), 
	    AttrType = lists:flatten([transform_attribute(Rtype) || Rtype <- Rt]), 
	    do_transform_2(AttrRec ++ AttrType, Forms, Context)
end.

do_transform_2(Attr, Forms, Context) ->
    ?dbg("Attr = ~p~n", [Attr]),
    Func = codegen:gen_function(?FUN_NAME, fun() -> {'$var', Attr} end),
    ?dbg("Func = ~p~n", [Func]),
    Forms2 = parse_trans:do_insert_forms(below, [Func], Forms, Context),
    ?dbg("Forms2 = ~p~n", [Forms2]),
    Forms3 = parse_trans:export_function(?FUN_NAME, 0, Forms2),
    ?dbg("Forms3 = ~p~n", [Forms3]),
    parse_trans:revert(Forms3).

transform_attribute({type, {TName, TDescr, []}}) ->
    {{type, TName}, typename(TDescr)};
transform_attribute(_NoMatch) ->
    ?dbg("transform_attribute/1 No match = ~p~n", [_NoMatch]),
    [].

transform_attribute_rec({RName, Fields}) ->
    ?dbg("Rname = ~p~n", [RName]),
    Rec = {{record, RName}, [field_info(F) || F <- Fields]},
    ?dbg("Rec = ~p~n", [Rec]),
    Rec;
transform_attribute_rec(_NoMatch) ->
    ?dbg("transform_attribute/1 No match = ~p~n", [_NoMatch]),
    [].

%% field has no type info
field_info({FieldName, none}) ->
    {FieldName, {atom, undefined}};
field_info({FieldName, {none, none}}) ->
    {FieldName, {atom, undefined}};
%% field has no type info, but it does have an initializer
field_info({FieldName, {none, {type, _, _Ty, _} = Type}}) ->
    {FieldName, typename(Type)};
field_info({FieldName, {none, {_, _, _, _} = Type}}) ->
    {FieldName, typename(Type)};
field_info({FieldName, {none, {_, _, _} = Type}}) ->
    {FieldName, typename(Type)};
field_info({FieldName, {{record, _, _, _}, Type}}) ->
    {FieldName, typename(Type)};
field_info({FieldName, {{_, _, _}, Type}}) ->
    {FieldName, typename(Type)}.

typename({var, _, Lit}) ->
    {literal, Lit};
typename({atom, _, Value}) ->
    {atom, Value};
typename({type, _, record, [{atom, _, RecType}]}) ->
    {record, RecType};
typename({type, _, list, [LType]}) ->
    {list, typename(LType)};
typename({type, _, Type, Subtypes}) when Type =:= union; Type =:= tuple ->
    {Type, [typename(T) || T <- Subtypes]};
typename({type, _, Type, Subtypes, []}) when Type =:= union; Type =:= tuple ->
    {Type, [typename(T) || T <- Subtypes]};
typename({type, _, TypeName, _}) ->
    {type, TypeName};
typename({user_type, _, TypeName, _}) ->
    {type, TypeName};
typename({remote_type, _, [{atom, _, Mod}, {atom, _, Type}, []]}) ->
    {type, {Mod, Type}};
typename({remote_type, _, [{atom, _, Mod}, {atom, _, Type}, TypeParam]}) ->
    {type, {Mod, Type, [typename(T) || T <- TypeParam]}};
typename({ann_type, _, [{var, _, _}, T]}) ->
    typename(T);
typename({A, _, _}) when is_atom(A) ->
    A.
