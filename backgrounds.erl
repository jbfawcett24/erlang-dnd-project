-module(backgrounds). % Changed module name to match the filename
-export([display_background/1, choose_background/0]).

-define(BACKGROUNDS, [ % Changed macro name to uppercase
    {"Acolyte", "INSIGHT, RELIGION"},
    {"Criminal/ Spy", "DECEPTION, SLEIGHT OF HAND"},
    {"Entertainer", "ACROBATICS, PERFORMANCE"},
    {"Folk Hero", "ANIMAL HANDLING, SURVIVAL"},
    {"Gladiator", "ACROBATICS, PERFORMANCE"},
    {"Guild Artisan/ Merchant", "INSIGHT, PERSUASION"},
    {"Hermit", "MEDICINE, RELIGION"},
    {"Knight", "HISTORY, PERSUASION"},
    {"Noble", "HISTORY, PERSUASION"}
]).

display_background(Name) ->
    % display the information for the input background
    case lists:keyfind(Name, 1, ?BACKGROUNDS) of % Updated macro usage
        {Name, Skills} -> 
            io:format("Background: ~s~nSkills: ~s~n", [Name, Skills]);
        false -> 
            io:format("Background not found.~n")
    end.

choose_background() ->
    % prompt the user to select a background

    lists:foreach(fun({Idx, {Name, _}}) ->
        io:format("~B. ~s~n", [Idx, Name])
    end, lists:zip(lists:seq(1, length(?BACKGROUNDS)), ?BACKGROUNDS)), % Updated macro usage

    io:format("Choose a background by typing its number: "),
    
    case io:fread("", "~d") of
        {ok, [Input]} when Input >= 1, Input =< length(?BACKGROUNDS) -> % Updated macro usage
            {ChosenBackground, Skills} = lists:nth(Input, ?BACKGROUNDS), % Updated macro usage
            io:format("You chose: ~s~nSkills: ~s~nConfirm? (y/n): ", [ChosenBackground, Skills]),
            
            case io:get_line("") of
                "y\n" -> 
                    io:format("Background confirmed: ~s~n", [ChosenBackground]),
                    {ChosenBackground, Skills};
                "n\n" -> 
                    % Start over
                    choose_background();
                _ ->