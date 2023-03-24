# HaskellDnD
My functional programming project, DnD characters and stats in Haskell.

Main issues now: Separating data out of the Equipment list within the CharacterStats data type so manipulate in combat equations.
--I looked into the Lens library, but had troubles trying to install it as a dependency initilly; eventually ran out of time to try learning.

Major Update3: Refactored EquippedEquipment into a triple, can be used as a tuple within equip. Still not working as expected; limited to 1 item which will no longer allow another item of a different type to be equipped.

Originally intended as a combat simulator, the submitted version instead focuses on equipment and the stats associated with a character, and that character's "equipped equipment" which may be nothing, or (ideally) one of each weapon, armor, and accessory. 

Equipment.hs
===============

Equipment is the base type class for all equipment, containing the stats as well as the "type" of equipment (Weapon, Armor, Accessory) as well as the information of that type of equipment (the name, stats, damage, AC.)

Each type has an initial Data declaration and subsequent Instance. Data focuses on the members of the class, while instance specifies how it interacts within the system. For example, the instance contains the equipmentType _ = "Weapon" armor/accessory so the functions know how to interperet each equipment type.

Equip is able to take any equipment type and is thus polymorphic. However KNOWN BUG: need to reimplement either the Character's 'equippedEquipment' member, or the equip function itself, to allow equipping multiple items with a maximum of 1 of each item.

Dice.hs:
===============
Creates a Data 'Dice' to implement rolling dice simultion. Most of my Functional Programming takes place in this file for practice, mainly implementing Dice as a fully functional monad.

Character.hs: 
===============
Old version of my character + equipment implementation; kept so it may be reviewed and compared for a history of how my implementation changed.

DNDTests.hs:
===============
My test suite. Tests various equip functions and dice; currently commented out test case for multiple items found the bug where characters can only have 1 item of 1 type and no more items.

Combat.hs:
===============
Was working on combat simulation, starting with comparing defender's AC to attackers 'DiceRoll + WepDmg" before realizing my bug where character's can only have 1 item at a time.

Main.hs:
===============
Just a vessel for running the project when initiating stack build and stack run.
