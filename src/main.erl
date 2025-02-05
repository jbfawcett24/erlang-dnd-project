-module(main).
-export(menu/0).

menu() ->
    %start the processes
    SpellsPID = spells:start_known_spells(),
    %Replace later, this is to get the character level
    io:format("Enter your character level: "),
    Input = string:trim(io:get_line("")),
    {Level, _} = string:to_integer(Input),
    set_known_spells(SpellsPid, Level),
    %display actions
    io:format("[1] View Class info"),
    io:format("[2] View Spell info"),
    io:format("[3] View Background Info"),

    Input = string:trim(io:get_line(""))
    


