-module(common).

-export([parse_file/1, apply_rules/3, tally_elements/1, polymerization/3]).

-import(lists, [append/2]).
-import(string, [chomp/1, split/2, slice/2, slice/3]).


% File parsing

parse_rule(Raw) ->
    [S, I] = split(chomp(Raw), " -> "),
    #{{slice(S, 0, 1), slice(S, 1, 1)} => I}.

parse_seq(Sequence, Acc) ->
    case slice(Sequence, 0, 1) of
        "" -> Acc;
        First -> parse_seq(slice(Sequence, 1), append(Acc, [First]))
    end.

parse_lines(File, Mode, Sequence, Rules) ->
    case {Mode, file:read_line(File)} of
        {read_seq, {ok, "\n"}} ->
            parse_lines(File, read_rules, Sequence, Rules);

        {read_seq, {ok, Val}} ->
            NextSequence = parse_seq(chomp(Val), []),
            parse_lines(File, read_seq, NextSequence, Rules);

        {read_rules, {ok, Val}} ->
            NextRules = maps:merge(Rules, parse_rule(Val)),
            parse_lines(File, read_rules, Sequence, NextRules);

        {read_rules, eof} ->
            {Sequence, Rules}
    end.

parse_file(Path) ->
    {ok, File} = file:open(Path, [read]),
    parse_lines(File, read_seq, [], #{}).


% Helper functions

each_cons2(List = [A, B | _], F) when is_function(F) ->
    Result = F(A, B),
    each_cons2(tl(List), [Result], F).

each_cons2([A, B], Acc, F) when is_function(F) ->
    Result = F(A, B),
    lists:append(Acc, [Result]);
each_cons2(List = [A, B | _], Acc, F) when is_function(F) ->
    Result = F(A, B),
    each_cons2(tl(List), lists:append(Acc, [Result]), F).

succ(V) -> V + 1.


% Slow algorithm

apply_rules(Sequence, _, 0) ->
    Sequence;
apply_rules(Sequence, Rules, Times) ->
    NextSequence = apply_rules(Sequence, Rules),
    apply_rules(NextSequence, Rules, Times - 1).

apply_rules(Sequence, Rules) ->
    Cons = each_cons2(Sequence, fun (A, B) ->
        [A, maps:get({A, B}, Rules), B]
    end),
    flatten_seq(Cons, []).

tally_elements(Sequence) ->
    tally_elements(Sequence, #{}).

tally_elements([], Acc) ->
    Acc;
tally_elements([Head | Tail], Acc) ->
    Tally = maps:put(Head, 1 + maps:get(Head, Acc, 0), Acc),
    tally_elements(Tail, Tally).

flatten_seq([[A, B, C]], Acc) ->
    lists:append(Acc, [A, B, C]);
flatten_seq([[A, B | _] | Tail], Acc) ->
    flatten_seq(Tail, lists:append(Acc, [A, B])).


% Fast algorithm

apply_rule(Pair, Rules) ->
    maps:get(Pair, Rules).

update_tally({A, R, B}, Occ, Tally) ->
    AddOccurences = fun(V) -> V + Occ end,
    Tmp = maps:update_with({A, R}, AddOccurences, Occ, Tally),
    maps:update_with({R, B}, AddOccurences, Occ, Tmp).

polymerization(Sequence, Rules, Times) when is_list(Sequence) ->
    Cons = each_cons2(Sequence, fun(A, B) -> {A, B} end),

    % first tally up all entries and build the counters
    {Tally, Counter} = lists:foldl(fun(Pair = {A, _}, {Tal, Cnt}) ->
        T = maps:update_with(Pair, fun succ/1, 1, Tal),
        C = maps:update_with(A, fun succ/1, 1, Cnt),
        {T, C}
    end, {#{},#{}}, Cons),

    % because the last one got skipped we have to add it manually
    {_, Last} = lists:last(Cons),
    Counter2 = maps:update_with(Last, fun succ/1, 1, Counter),

    polymerization(Rules, Tally, Counter2, Times).

polymerization(_, _, Counter, 0) ->
    Counter;

polymerization(Rules, Tally, Counter, Times) ->
    {NewTally, NewCounter} = polymerization_step(Rules, Tally, Counter),
    polymerization(Rules, NewTally, NewCounter, Times - 1).

polymerization_step(Rules, Tally, Counter) ->
    maps:fold(fun({A, B}, Occ, {NewTally, NewCounter}) ->
        Re = apply_rule({A, B}, Rules),

        % Update occurance and element counters
        NewTally2 = update_tally({A, Re, B}, Occ, NewTally),
        NewCounter2 = maps:update_with(Re, fun(V) -> V + Occ end, 1, NewCounter),

        {NewTally2, NewCounter2}
    end, {#{}, Counter}, Tally).
