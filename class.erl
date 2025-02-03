-module(class).
-export([get_class/1]).

-record(class, {
    name,
    hit_die,
    saving_throws,
    abilities % List of {Ability, Level}
}).

get_class(Class) ->
    case Class of
        get_class(barbarian) ->
            #class{
                name = "Barbarian",
                hit_die = "d12",
                saving_throws = ["Strength", "Constitution"],
                abilities = [{"Rage", 1}, {"Unarmored Defense", 1}, {"Reckless Attack", 2}, {"Danger Sense", 2} {"Primal Path", 3} {"Extra Attack", 5}]
            };
        get_class(bard) ->
            #class{
                name = "Bard",
                hit_die = "d8",
                saving_throws = ["Dexterity", "Charisma"],
                abilities = [{"Bardic Inspiration", 1}, {"Jack of All Trades", 2}, {"Song of Rest", 2}, {"Bard College", 3}{"Expertise", 3}{"bardic Inspiration", 5} {"Font of Inspiration", 5}]
            };
        get_class(cleric) ->
            #class{
                name = "Cleric",
                hit_die = "d8",
                saving_throws = ["Wisdom", "Charisma"],
                abilities = [{"Divine Domain", 1}, {"Channel Divinity", 2},{"destroy undead", 5} {"Divine Intervention", 10}]
            };
        get_class(druid) ->
            #class{
                name = "Druid",
                hit_die = "d8",
                saving_throws = ["Intelligence", "Wisdom"],
                abilities = [{"Druidic", 1}, {"Wild Shape", 2}, {"Spellcasting", 1}{"Druid Circle", 2}{"Wild Shape improvement", 4}]
            };
        get_class(fighter) ->
            #class{
                name = "Fighter",
                hit_die = "d10",
                saving_throws = ["Strength", "Constitution"],
                abilities = [{"Fighting Style", 1}, {"Second Wind", 1}, {"Action Surge", 2}, {"Martial Archetype", 3} {"Extra Attack", 5}]
            };
        get_class(monk) ->
            #class{
                name = "Monk",
                hit_die = "d8",
                saving_throws = ["Strength", "Dexterity"],
                abilities = [{"Martial Arts", 1},{"Unarmored Defense",1} {"Ki", 2}, {"Unarmored Movement", 2}, {"Monastic Tradition", 3},{"deflect Missiles", 3}{"Slow Fall", 4}{"Stunning Strike", 5}]
            };
        get_class(paladin) ->
            #class{
                name = "Paladin",
                hit_die = "d10",
                saving_throws = ["Wisdom", "Charisma"],
                abilities = [{"Divine Sense", 1}, {"Lay on Hands", 1}, {"Fighting Style", 2}, {"Divine Smite", 2},{"Divine Health", 3}{"Sacred Oath", 3}{"Extra Attack", 5}]
            };
        get_class(ranger) ->
            #class{
                name = "Ranger",
                hit_die = "d10",
                saving_throws = ["Strength", "Dexterity"],
                abilities = [{"Favored Enemy", 1}, {"Natural Explorer", 1}, {"Fighting Style", 2}, {"Spellcasting", 2},{"Ranger Archetype", 3}{"Primeval Awareness", 3}{"Extra Attack", 5}]
            };
        get_class(rogue) ->
            #class{
                name = "Rogue",
                hit_die = "d8",
                saving_throws = ["Dexterity", "Intelligence"],
                abilities = [{"Sneak Attack", 1}, {"Cunning Action", 2}, {"Roguish Archetype", 3}, {"Uncanny Dodge", 5}]
            };
        get_class(sorcerer) ->
            #class{
                name = "Sorcerer",
                hit_die = "d6",
                saving_throws = ["Constitution", "Charisma"],
                abilities = [{"Spellcasting", 1}, {"Sorcerous Origin", 1}, {"Font of Magic", 2}, {"Metamagic", 3},]
            };
        get_class(warlock) ->
            #class{
                name = "Warlock",
                hit_die = "d8",
                saving_throws = ["Wisdom", "Charisma"],
                abilities = [{"Otherworldly Patron", 1}, {"Pact Magic", 1}, {"Eldritch Invocations", 2}, {"Pact Boon", 3}]
            };
        get_class(wizard) ->
            #class{
                name = "Wizard",
                hit_die = "d6",
                saving_throws = ["Intelligence", "Wisdom"],
                abilities = [{"Spellcasting", 1}, {"Arcane Recovery", 1}, {"Arcane Tradition", 2}]
            }.
        _ -> 
            {error, unknown_class}
    end.
-module(dnd_project).
-export([class_menu/0, get_ability_info/2, display_ability/0]).

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
    {"Font of Inspiration", 5, "Regain all of your expended uses of Bardic Inspiration when you finish a short or long rest"},
    {"Divine Domain", 1, "Choose a divine domain related to your deity"},
    {"Channel Divinity", 2, "Use your Channel Divinity to fuel magical effects"},
    {"Destroy Undead", 5, "When an undead fails its saving throw against your Turn Undead feature, it is instantly destroyed"},
    {"Divine Intervention", 10, "Call on your deity to intervene on your behalf when your need is great"},
    {"Druidic", 1, "You know Druidic, the secret language of druids"},
    {"Wild Shape", 2, "Use your action to magically assume the shape of a beast that you have seen before"},
    {"Spellcasting", 1, "You can cast prepared druid spells using your spell slots"},
    {"Druid Circle", 2, "Choose a circle of druids to align with"},
    {"Wild Shape Improvement", 4, "Your ability to transform into beasts improves"},
    {"Fighting Style", 1, "Adopt a particular style of fighting as your specialty"},
    {"Second Wind", 1, "Use a bonus action to regain hit points equal to 1d10 + your fighter level"},
    {"Action Surge", 2, "Take one additional action on your turn"},
    {"Martial Archetype", 3, "Choose an archetype that you strive to emulate in your combat styles and techniques"},
    {"Martial Arts", 1, "Use Dexterity instead of Strength for the attack and damage rolls of your unarmed strikes and monk weapons"},
    {"Ki", 2, "Harness the mystic energy of ki to perform extraordinary feats"},
    {"Unarmored Movement", 2, "Your speed increases by 10 feet while you are not wearing armor or wielding a shield"},
    {"Monastic Tradition", 3, "Choose a monastic tradition that shapes your practice of martial arts"},
    {"Deflect Missiles", 3, "Use your reaction to deflect or catch the missile when you are hit by a ranged weapon attack"},
    {"Slow Fall", 4, "Use your reaction when you fall to reduce any falling damage you take by an amount equal to five times your monk level"},
    {"Stunning Strike", 5, "Interfere with the flow of ki in an opponent's body to stun them"},
    {"Divine Sense", 1, "Sense the presence of strong evil or powerful good"},
    {"Lay on Hands", 1, "Heal a creature you touch by transferring your pool of healing power"},
    {"Divine Smite", 2, "Expend a spell slot to deal radiant damage to a target you hit with a melee weapon attack"},
    {"Divine Health", 3, "You are immune to disease"},
    {"Sacred Oath", 3, "Swear the oath that binds you as a paladin forever"},
    {"Favored Enemy", 1, "You have significant experience studying, tracking, hunting, and even talking to a certain type of enemy"},
    {"Natural Explorer", 1, "You are particularly familiar with one type of natural environment and are adept at traveling and surviving in such regions"},
    {"Primeval Awareness", 3, "Use your action and expend one ranger spell slot to focus your awareness on the region around you"},
    {"Sneak Attack", 1, "Deal extra damage to one creature you hit with an attack if you have advantage on the attack roll"},
    {"Cunning Action", 2, "Use your bonus action to take the Dash, Disengage, or Hide action"},
    {"Roguish Archetype", 3, "Choose an archetype that you emulate in the exercise of your rogue abilities"},
    {"Uncanny Dodge", 5, "Use your reaction to halve the attack's damage against you"},
    {"Sorcerous Origin", 1, "Choose a sorcerous origin, which describes the source of your innate magical power"},
    {"Font of Magic", 2, "Tap into a deep wellspring of magic within yourself"},
    {"Metamagic", 3, "Shape your spells to suit your needs"},
    {"Otherworldly Patron", 1, "Choose an otherworldly patron, which grants you magical abilities"},
    {"Pact Magic", 1, "Learn pact magic, which allows you to cast spells"},
    {"Eldritch Invocations", 2, "Learn eldritch invocations, which are magical abilities granted by your patron"},
    {"Pact Boon", 3, "Choose a boon granted by your patron"},
    {"Arcane Recovery", 1, "Regain some of your magical energy by studying your spellbook"},
    {"Arcane Tradition", 2, "Choose an arcane tradition, which shapes your practice of magic"}
]).

-define(known_abilities, [
    "Rage",
    "Unarmored Defense",
    "Reckless Attack",
    "Danger Sense",
    "Primal Path",
    "Extra Attack",
    "Bardic Inspiration",
    "Jack of All Trades",
    "Song of Rest",
    "Bard College",
    "Expertise",
    "Font of Inspiration",
    "Divine Domain",
    "Channel Divinity",
    "Destroy Undead",
    "Divine Intervention",
    "Druidic",
    "Wild Shape",
    "Spellcasting",
    "Druid Circle",
    "Wild Shape Improvement",
    "Fighting Style"
]).

class_menu() -> 
    % Spawn the display_ability process
    Pid = spawn(?MODULE, display_ability, []),

    % Display the list of known abilities
    io:format("Known Abilities:~n"),
    lists:foreach(
        fun({Index, Name}) ->
            io:format("~p) ~s~n", [Index, Name])
        end,
        lists:zip(lists:seq(1, length(?known_abilities)), ?known_abilities)
    ),

    % Prompt for user input
    io:format("Enter the ability number to view details: "),
    Input = string:trim(io:get_line("")),
    get_ability_info(Pid, Input),
    class_menu().

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
