-module(class).
-export([get_class/1, class_menu/0, display_class_info/1, get_ability_info/2, display_ability/0, ability_menu/2]).

-record(class, {
    name,
    hit_die,
    saving_throws,
    abilities % List of {Ability, Level}
}).

-define(abilities, [
    {"Rage", 1, "Enter a rage, gaining bonuses to damage and resistance to damage"},
    {"Unarmored Defense", 1, "Calculate AC using Dexterity and Constitution"},
    {"Reckless Attack", 2, "Gain advantage on melee attacks using Strength"},
    {"Danger Sense", 2, "Gain advantage on Dexterity saving throws against effects you can see"},
    {"Primal Path", 3, "Choose a path that shapes the nature of your rage"},
    {"Extra Attack", 5, "Attack twice whenever you take the Attack action"},
    {"Bardic Inspiration", 1, "Use a bonus action to grant a Bardic Inspiration die to a creature"},
    {"Jack of All Trades", 2, "Add half your proficiency bonus to any ability check you make that doesn't already include your proficiency bonus"},
    {"Song of Rest", 2, "Use soothing music or oration to help revitalize your wounded allies during a short rest"},
    {"Bard College", 3, "Choose a bard college that shapes your practice of bardic magic"},
    {"Expertise", 3, "Choose two of your skill proficiencies, or one of your skill proficiencies and your proficiency with thieves' tools"},
    {"Font of Inspiration", 5, "Regain all of your expended uses of Bardic Inspiration when you finish a short or long rest"}
]).

-spec class_menu() -> ok.
class_menu() ->
    Pid = spawn(?MODULE, display_ability, []),

    io:format("Enter class name (e.g., barbarian, bard, cleric): "),
    Input = string:trim(io:get_line("")),

    case get_class(Input) of
        {error, unknown_class} ->
            io:format("Unknown class. Try again.~n"),
            class_menu();
        Class ->
            io:format("Class record in class_menu: ~p~n", [Class]),
            display_class_info(Class),
            ability_menu(Pid, Class)
    end.


-spec ability_menu(Pid :: pid(), Class :: #class{}) -> ok.
ability_menu(Pid, Class) ->
    io:format("Entering ability_menu with Class: ~p~n", [Class]),
    io:format("Abilities List in ability_menu: ~p~n", [Class#class.abilities]),

    io:format("Enter the ability number to view details (or 'exit' to return to class selection): "),
    RawInput = io:get_line(""),
    TrimmedInput = string:trim(RawInput),

    io:format("Raw Input: ~p~n", [RawInput]),
    io:format("Trimmed Input: ~p~n", [TrimmedInput]),

    case TrimmedInput of
        "exit" -> class_menu();
        _ ->
            case string:to_integer(TrimmedInput) of
                {ok, Index} when Index >= 1, Index =< length(Class#class.abilities) ->
                    case lists:nth(Index, Class#class.abilities) of
                        {AbilityName, _Level} = AbilityTuple ->  % Explicitly bind and match
                            io:format("Selected Ability: ~p~n", [AbilityTuple]),
                            get_ability_info(Pid, AbilityName),
                            ability_menu(Pid, Class);
                        _Other ->
                            io:format("Unexpected Error: Invalid ability format: ~p~n", [_Other]),
                            ability_menu(Pid, Class)
                    end;
                {ok, _} ->
                    io:format("Invalid number. Try again.~n"),
                    ability_menu(Pid, Class);
                {error, _} ->
                    io:format("Invalid input. Enter a number.~n"),
                    ability_menu(Pid, Class)
            end
    end.




-spec display_class_info(Class :: #class{}) -> ok.
display_class_info(Class) ->
    io:format("~nClass: ~s~nHit Die: ~s~nSaving Throws: ~p~n", 
              [Class#class.name, Class#class.hit_die, Class#class.saving_throws]),
    io:format("Abilities:~n"),
    lists:foreach(
        fun({Index, {Ability, Level}}) ->
            io:format("~p) ~s (Level ~p)~n", [Index, Ability, Level])
        end,
        lists:zip(lists:seq(1, length(Class#class.abilities)), Class#class.abilities)
    ),
    ok.

-spec display_ability() -> ok.
display_ability() ->
    receive
        {Name, undefined, "Ability not found"} ->
            io:format("Ability '~s' not found.~n", [Name]),
            display_ability();
        {Name, Level, Description} ->
            io:format("Name: ~s~nLevel: ~p~nDescription: ~s~n", [Name, Level, Description]),
            display_ability()
    end.

-spec get_ability_info(Pid :: pid(), Name :: string()) -> ok.
get_ability_info(Pid, Name) ->
    case lists:keyfind(Name, 1, ?abilities) of
        {Name, Level, Description} ->
            Pid ! {Name, Level, Description};
        false ->
            Pid ! {Name, undefined, "Ability not found"}
    end,
    ok.

-spec get_class(ClassName :: string()) -> {error, unknown_class} | #class{}.
get_class("barbarian") ->
    #class{
        name = "Barbarian",
        hit_die = "d12",
        saving_throws = ["Strength", "Constitution"],
        abilities = [{"Rage", 1}, {"Unarmored Defense", 1}, {"Reckless Attack", 2}, {"Danger Sense", 2}, {"Primal Path", 3}, {"Extra Attack", 5}]
    };
get_class("bard") ->
    #class{
        name = "Bard",
        hit_die = "d8",
        saving_throws = ["Dexterity", "Charisma"],
        abilities = [{"Bardic Inspiration", 1}, {"Jack of All Trades", 2}, {"Song of Rest", 2}, {"Bard College", 3}, {"Expertise", 3}, {"Font of Inspiration", 5}]
    };
get_class("cleric") ->
    #class{
        name = "Cleric",
        hit_die = "d8",
        saving_throws = ["Wisdom", "Charisma"],
        abilities = [{"Divine Domain", 1}, {"Channel Divinity", 2}, {"Destroy Undead", 5}, {"Divine Intervention", 10}]
    };
get_class(_) -> 
    {error, unknown_class}.
