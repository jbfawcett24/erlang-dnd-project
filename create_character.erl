-module(create_character).
-export([create_character/4, build_character/0, save_character/2, load_character/1]).

% Importing necessary modules
-import(race, [get_race_list/0]).
-import(class, [get_class_list/0]).
-import(spells, [get_spell_list/1]).

% Function to create a character
create_character(Name, Level, Race, Class) ->
    RaceList = get_race_list(),
    ClassList = get_class_list(),
    
    % Check if the provided race and class are valid
    case lists:member(Race, RaceList) of
        true ->
            case lists:member(Class, ClassList) of
                true ->
                    % If the class is a spellcaster, get the spell list
                    Spells = case is_spellcaster(Class) of
                        true -> get_spell_list(Class);
                        false -> []
                    end,
                    % Return the character details
                    {ok, #{name => Name, level => Level, race => Race, class => Class, spells => Spells}};
                false -> {error, invalid_class}
            end;
        false -> {error, invalid_race}
    end.

% Helper function to determine if a class is a spellcaster
is_spellcaster(Class) ->
    SpellcasterClasses = ["Wizard", "Sorcerer", "Cleric", "Druid", "Bard"], % Add other spellcaster classes as needed
    lists:member(Class, SpellcasterClasses).

% Function to build a character by prompting the user for input
build_character() ->
    io:format("Enter character name: "),
    Name = io:get_line(""),
    io:format("Enter character level: "),
    {ok, [Level]} = io:fread("", "~d"),
    io:format("Enter character race: "),
    Race = io:get_line(""),
    io:format("Enter character class: "),
    Class = io:get_line(""),
    
    % Trim newline characters from input
    NameTrimmed = string:trim(Name),
    RaceTrimmed = string:trim(Race),
    ClassTrimmed = string:trim(Class),
    
    % Create the character
    case create_character(NameTrimmed, Level, RaceTrimmed, ClassTrimmed) of
        {ok, Character} ->
            io:format("Character created successfully: ~p~n", [Character]),
            % Prompt for filename to save the character
            io:format("Enter filename to save the character: "),
            Filename = string:trim(io:get_line("")),
            save_character(Filename, Character),
            io:format("Character saved successfully to ~s~n", [Filename]);
        {error, Reason} ->
            io:format("Error creating character: ~p~n", [Reason])
    end.

% Function to save a character to a file
save_character(Filename, Character) ->
    {ok, File} = file:open(Filename, [write]),
    io:format(File, "~p.~n", [Character]),
    file:close(File),
    ok.

% Function to load a character from a file
load_character(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary)),
            {ok, [Character]} = erl_parse:parse_term(Tokens),
            {ok, Character};
        {error, Reason} ->
            {error, Reason}
    end.