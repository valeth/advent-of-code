-module(part_two).

-export([main/1]).

-import(common, [parse_file/1, polymerization/3]).

main(Args) ->
    Path = lists:last(Args),
    {Sequence, Rules} = parse_file(Path),

    Counter = polymerization(Sequence, Rules, 40),
    Values = maps:values(Counter),
    Max = lists:max(Values),
    Min = lists:min(Values),

    io:format("~p~n", [Max - Min]).
