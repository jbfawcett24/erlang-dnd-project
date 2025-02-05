-module(race).
-export([get_race/1]).

-record(race, {
    name,
    speed,
    abilities, % List of {Ability, Level}
    ability_score_improvements % List of {Ability, Improvement}
}).

get_race(Race) ->
    case Race of
        get_race(human) ->
            #race{
                name = "Human",
                speed = 30,
                abilities = [{"Versatile", 1}],
                ability_score_improvements = [{"All", 1}]
            };
        get_race(high_elf) ->
            #race{
                name = "High Elf",
                speed = 30,
                abilities = [{"Darkvision", 1}, {"Keen Senses", 1}, {"Fey Ancestry", 1}, {"Trance", 1}, {"Elf Weapon Training", 1}],
                ability_score_improvements = [{"Dexterity", 2}, {"Intelligence", 1}]
            };
        get_race(wood_elf) ->
            #race{
                name = "Wood Elf",
                speed = 35,
                abilities = [{"Darkvision", 1}, {"Keen Senses", 1}, {"Fey Ancestry", 1}, {"Trance", 1}, {"Elf Weapon Training", 1}, {"Fleet of Foot", 1}, {"Mask of the Wild", 1}],
                ability_score_improvements = [{"Dexterity", 2}, {"Wisdom", 1}]
            };
        get_race(dark_elf) ->
            #race{
                name = "Dark Elf (Drow)",
                speed = 30,
                abilities = [{"Superior Darkvision", 1}, {"Keen Senses", 1}, {"Fey Ancestry", 1}, {"Trance", 1}, {"Drow Magic", 1}, {"Sunlight Sensitivity", 1}],
                ability_score_improvements = [{"Dexterity", 2}, {"Charisma", 1}]
            };
        get_race(hill_dwarf) ->
            #race{
            name = "Hill Dwarf",
            speed = 25,
            abilities = [{"Darkvision", 1}, {"Dwarven Resilience", 1}, {"Dwarven Combat Training", 1}, {"Stonecunning", 1}, {"Dwarven Toughness", 1}],
            ability_score_improvements = [{"Constitution", 2}, {"Wisdom", 1}]
            };
        get_race(mountain_dwarf) ->
            #race{
            name = "Mountain Dwarf",
            speed = 25,
            abilities = [{"Darkvision", 1}, {"Dwarven Resilience", 1}, {"Dwarven Combat Training", 1}, {"Stonecunning", 1}, {"Dwarven Armor Training", 1}],
            ability_score_improvements = [{"Constitution", 2}, {"Strength", 2}]
            }
            };
        get_race(lightfoot_halfling) ->
            #race{
            name = "Lightfoot Halfling",
            speed = 25,
            abilities = [{"Lucky", 1}, {"Brave", 1}, {"Halfling Nimbleness", 1}, {"Naturally Stealthy", 1}],
            ability_score_improvements = [{"Dexterity", 2}, {"Charisma", 1}]
            };
        get_race(stout_halfling) ->
            #race{
            name = "Stout Halfling",
            speed = 25,
            abilities = [{"Lucky", 1}, {"Brave", 1}, {"Halfling Nimbleness", 1}, {"Stout Resilience", 1}],
            ability_score_improvements = [{"Dexterity", 2}, {"Constitution", 1}]
            };
        get_race(dragonborn) ->
            #race{
                name = "Dragonborn",
                speed = 30,
                abilities = [{"Draconic Ancestry", 1}, {"Breath Weapon", 1}, {"Damage Resistance", 1}],
                ability_score_improvements = [{"Strength", 2}, {"Charisma", 1}]
            };
        get_race(forest_gnome) ->
            #race{
            name = "Forest Gnome",
            speed = 25,
            abilities = [{"Darkvision", 1}, {"Gnome Cunning", 1}, {"Natural Illusionist", 1}, {"Speak with Small Beasts", 1}],
            ability_score_improvements = [{"Intelligence", 2}, {"Dexterity", 1}]
            };
        get_race(rock_gnome) ->
            #race{
            name = "Rock Gnome",
            speed = 25,
            abilities = [{"Darkvision", 1}, {"Gnome Cunning", 1}, {"Artificer's Lore", 1}, {"Tinker", 1}],
            ability_score_improvements = [{"Intelligence", 2}, {"Constitution", 1}]
            };
        get_race(half_elf) ->
            #race{
                name = "Half-Elf",
                speed = 30,
                abilities = [{"Darkvision", 1}, {"Fey Ancestry", 1}, {"Skill Versatility", 1}],
                ability_score_improvements = [{"Charisma", 2}, {"Any", 1}, {"Any", 1}]
            };
        get_race(half_orc) ->
            #race{
                name = "Half-Orc",
                speed = 30,
                abilities = [{"Darkvision", 1}, {"Menacing", 1}, {"Relentless Endurance", 1}, {"Savage Attacks", 1}],
                ability_score_improvements = [{"Strength", 2}, {"Constitution", 1}]
            };
        get_race(tiefling) ->
            #race{
                name = "Tiefling",
                speed = 30,
                abilities = [{"Darkvision", 1}, {"Hellish Resistance", 1}, {"Infernal Legacy", 1}],
                ability_score_improvements = [{"Charisma", 2}, {"Intelligence", 1}]
            };
        _ -> 
            {error, unknown_race}
    end.


get_ability_description(Ability) ->
    case Ability of
        "Versatile" -> "You gain proficiency in one skill of your choice.";
        "Darkvision" -> "You can see in darkness (shades of gray) up to 60 feet.";
        "Keen Senses" -> "You have proficiency in the Perception skill.";
        "Fey Ancestry" -> "You have advantage on saving throws against being charmed, and magic can’t put you to sleep.";
        "Trance" -> "Elves don’t need to sleep. Instead, they meditate deeply for 4 hours a day.";
        "Elf Weapon Training" -> "You have proficiency with the longsword, shortsword, shortbow, and longbow.";
        "Fleet of Foot" -> "Your base walking speed increases to 35 feet.";
        "Mask of the Wild" -> "You can attempt to hide even when you are only lightly obscured by foliage, heavy rain, falling snow, mist, and other natural phenomena.";
        "Superior Darkvision" -> "Your darkvision has a radius of 120 feet.";
        "Drow Magic" -> "You know the dancing lights cantrip. At 3rd level, you can cast faerie fire once per day. At 5th level, you can also cast darkness once per day.";
        "Sunlight Sensitivity" -> "You have disadvantage on attack rolls and on Wisdom (Perception) checks that rely on sight when you, the target of your attack, or whatever you are trying to perceive is in direct sunlight.";
        "Dwarven Resilience" -> "You have advantage on saving throws against poison, and you have resistance against poison damage.";
        "Dwarven Combat Training" -> "You have proficiency with the battleaxe, handaxe, throwing hammer, and warhammer.";
        "Stonecunning" -> "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check.";
        "Lucky" -> "When you roll a 1 on the d20 for an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll.";
        "Brave" -> "You have advantage on saving throws against being frightened.";
        "Halfling Nimbleness" -> "You can move through the space of any creature that is of a size larger than yours.";
        "Naturally Stealthy" -> "You can attempt to hide even when you are obscured only by a creature that is at least one size larger than you.";
        "Stout Resilience" -> "You have advantage on saving throws against poison, and you have resistance against poison damage.";
        "Draconic Ancestry" -> "You have draconic ancestry. Choose one type of dragon from the Draconic Ancestry table. Your breath weapon and damage resistance are determined by the dragon type, as shown in the table.";
        "Breath Weapon" -> "You can use your action to exhale destructive energy. Your draconic ancestry determines the size, shape, and damage type of the exhalation.";
        "Damage Resistance" -> "You have resistance to the damage type associated with your draconic ancestry.";
        "Gnome Cunning" -> "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic.";
        "Skill Versatility" -> "You gain proficiency in two skills of your choice.";
        "Menacing" -> "You gain proficiency in the Intimidation skill.";
        "Relentless Endurance" -> "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead. You can’t use this feature again until you finish a long rest.";
        "Savage Attacks" -> "When you score a critical hit with a melee weapon attack, you can roll one of the weapon’s damage dice one additional time and add it to the extra damage of the critical hit.";
        "Hellish Resistance" -> "You have resistance to fire damage.";
        "Infernal Legacy" -> "You know the thaumaturgy cantrip. When you reach 3rd level, you can cast hellish rebuke once as a 2nd-level spell. When you reach 5th level, you can cast darkness once. You must finish a long rest to cast these spells again with this trait.";
        "Naturally Stealthy" -> "You can attempt to hide even when you are obscured only by a creature that is at least one size larger than you.";
        "Stout Resilience" -> "You have advantage on saving throws against poison, and you have resistance against poison damage.";
        _ -> "Unknown ability."
    end.