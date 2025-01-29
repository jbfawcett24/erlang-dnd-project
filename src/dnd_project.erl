-module(dnd_project).
-export([menu/0, get_spell_info/2, display_spell/0]).

-define(SPELLS, [
    {"Fireball", 5, "A fiery spell that causes damage"},
    {"Lightning Bolt", 8, "A powerful bolt of lightning"},
    {"Heal", 3, "A spell to restore health"}
]).

-define(known_spells, [
    "Fireball",
    "Heal"
]).

menu() ->
    % Spawn the display_spell process
    Pid = spawn(?MODULE, display_spell, []),

    % Display the list of known spells
    io:format("Known Spells:~n"),
    lists:foreach(
        fun({Index, Name}) ->
            io:format("~p) ~s~n", [Index, Name])
        end,  % <- This "end" is correct here.
        lists:zip(lists:seq(1, length(?known_spells)), ?known_spells)
    ),

    % Prompt for user input
    io:format("Enter the spell number to view details: "),
    Input = string:trim(io:get_line("")),
    get_spell_info(Pid, Input).
    % No extra "end." at the end of the function



-spec display_spell() -> ok.
display_spell() ->
    receive
        {Name, undefined, "Spell not found"} ->
            io:format("Spell '~s' not found.~n", [Name]),
            display_spell();
        {Name, Level, Description} ->
            io:format("Name: ~s~nLevel: ~p~nDescription: ~s~n", [Name, Level, Description]),
            display_spell()
    end.

-spec get_spell_info(Pid :: pid(), Name :: string()) -> ok.
get_spell_info(Pid, Name) ->
    case lists:keyfind(Name, 1, ?SPELLS) of
        {Name, Level, Description} ->
            Pid ! {Name, Level, Description};
        false ->
            Pid ! {Name, undefined, "Spell not found"}
    end,
    ok.
