-module(ice_shell).
-export([start/0]).

start() ->
    error_logger:tty(false),
    loop([]).

loop(Defs) ->
    case io:get_line("ICE> ") of
        eof -> init:stop();
        {error, _} -> init:stop();
        Line ->
            do_line(Line, Defs)
    end.

do_line(L, Defs) ->
    case check_line(L) of
        empty ->
            loop(Defs);
        {def, {Name, _} = D} ->
            NewDefs = lists:keystore(Name, 1, Defs, D),
            loop(NewDefs);
        {query, Q} ->
            Query = Q ++ where_defs(Defs),
            try
                io:format("~p\n", [ice:i(Query)])
            catch _:E ->
                io:format("*** ~p in ~s", [E, Query]),
                catch mnesia:stop() %% yuck!
            end,
            loop(Defs);
        {command, d} ->
            loop([]);
        {command, q} ->
            init:stop();
        {command, p} ->
            print(Defs),
            loop(Defs);
        {command, {l, Name}} ->
            loop(load(Name, Defs));
        {badcommand, C} ->
            io:format("*** Bad command: `~s'\n", [C]),
            loop(Defs)
    end.

check_line(S) ->
    case re:run(S, "^\\s*(//.*)?$") of
        nomatch ->
            check_command(S);
        _ ->
            empty
    end.

check_command(S) ->
    case re:run(S, "^:(\\w+)\\s+(.*)", [{capture, [1,2], list}]) of
        {match, ["d", ""]} ->
            {command, d};
        {match, ["p", ""]} ->
            {command, p};
        {match, ["q", ""]} ->
            {command, q};
        {match, ["l", File]} ->
            {command, {l, File}};
        {match, [C|_]} ->
            {badcommand, C};
        nomatch ->
            check_def(S)
    end.

check_def(S) ->
    case re:run(S, "^\\s*(fun|var|dim)\\s+(\\w+)", [{capture, [2],list}]) of
        {match, [Name]} ->
            {def, {Name, S}};
        nomatch ->
            {query, S}
    end.

load(Name, OrigDefs) ->
    try
        {ok, Bin} = file:read_file(Name),
        lists:foldl(fun do_file_line/2,
                    OrigDefs,
                    [S ++ "\n" || S <- string:tokens(binary_to_list(Bin), "\n")])
    catch _ : E ->
            io:format("*** error while loading  `~s': ~p\n", [Name, E]),
            OrigDefs
    end.

do_file_line(L, Defs) ->
    case check_line(L) of
        empty ->
            Defs;
        {def, {Name, _} = D} ->
            lists:keystore(Name, 1, Defs, D);
        Other ->
            throw(Other)
    end.

where_defs(Defs) ->
    XDefs = [Def || {_Name, Def} <- Defs],
    lists:flatten(["  where\n\t", string:join(XDefs, "\t"), "  end\n"]).

print(Defs) ->
    [io:format("~s", [Def]) || {_Name, Def} <- Defs],
    ok.
