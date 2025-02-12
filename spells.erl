-module(spells).
-export([spell_menu/0, get_spell_info/2, display_spell/0, set_known_spells/2, start_known_spells/0, get_known_spells/1, known_spells_loop/1]).

-define(SPELLS, [
    {"Acid Splash", 0, "60 feet", "You hurl a bubble of acid. Choose one creature you can see within range, or choose two creatures you can see within range that are within 5 feet of each other. A target must succeed on a Dexterity saving throw or take 1d6 acid damage."},
    {"Burning Hands", 1, "Self (15-foot cone)", "As you hold your hands with thumbs touching and fingers spread, a thin sheet of flames shoots forth from your outstretched fingertips. Each creature in a 15-foot cone must make a Dexterity saving throw. A creature takes 3d6 fire damage on a failed save, or half as much damage on a successful one."},
    {"Cloud of Daggers", 2, "60 feet", "You fill the air with spinning daggers in a cube 5 feet on each side, centered on a point you choose within range. A creature takes 4d4 slashing damage when it enters the spell’s area for the first time on a turn or starts its turn there."}
]).

%%% Starts the known spells process
start_known_spells() ->
    spawn(?MODULE, known_spells_loop, [[]]).

%%% Process to store known spells
known_spells_loop(KnownSpells) ->
    receive
        {set, Level, Caller} ->
            MaxLevel = (Level + 1) div 2,
            NewKnownSpells = [Name || {Name, SpellLevel, _, _} <- ?SPELLS, SpellLevel =< MaxLevel],
            Caller ! {ok, NewKnownSpells},
            known_spells_loop(NewKnownSpells);
        {get, Caller} ->
            Caller ! {ok, KnownSpells},
            known_spells_loop(KnownSpells)
    end.

%%% Set known spells based on level
set_known_spells(SpellsPid, Level) ->
    SpellsPid ! {set, Level, self()},
    receive
        {ok, NewSpells} -> NewSpells
    after 1000 -> timeout
    end.

%%% Get known spells
get_known_spells(SpellsPid) ->
    SpellsPid ! {get, self()},
    receive
        {ok, KnownSpells} -> KnownSpells
    after 1000 -> timeout
    end.

%%% Main Menu
spell_menu() ->
    SpellsPid = start_known_spells(),  % Start known spells process % Set spells based on level

    loop_menu(SpellsPid).  % Enter menu loop

%%% Menu loop to display spells and handle user input
loop_menu(SpellsPid) ->
    KnownSpells = get_known_spells(SpellsPid),

    io:format("~nKnown Spells:~n"),
    lists:foreach(
        fun({Index, Name}) ->
            io:format("~p) ~s~n", [Index, Name])
        end,
        lists:zip(lists:seq(1, length(KnownSpells)), KnownSpells)
    ),

    io:format("Enter the spell number to view details (or 'exit' to quit): "),
    Input = string:trim(io:get_line("")),
    
    case Input of
        "exit" -> io:format("Exiting spell menu.~n");
        _ -> 
            case string:to_integer(Input) of
                {Index, _} when Index > 0, Index =< length(KnownSpells) ->
                    SpellName = lists:nth(Index, KnownSpells),
                    Pid = spawn(?MODULE, display_spell, []),
                    get_spell_info(Pid, SpellName),
                    loop_menu(SpellsPid);
                _ ->
                    io:format("Invalid input, please try again.~n"),
                    loop_menu(SpellsPid)
            end
    end.

%%% Display spell details
display_spell() ->
    receive
        {Name, undefined, "Spell not found"} ->
            io:format("Spell '~s' not found.~n", [Name]),
            display_spell();
        {Name, Level, Range, Description} ->
            io:format("~nName: ~s~nLevel: ~p~nRange: ~s~nDescription: ~s~n", [Name, Level, Range, Description])
    end.

%%% Fetch spell information and send it to the display process
get_spell_info(Pid, Name) ->
    case lists:keyfind(Name, 1, ?SPELLS) of
        {Name, Level, Range, Description} ->
            Pid ! {Name, Level, Range, Description};
        false ->
            Pid ! {Name, undefined, "Spell not found"}
    end,
    ok.