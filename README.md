# HaskellDnD
My functional programming project, DnD characters and stats in Haskell.

Major Update1 (following comments also found in Main.hs):

--Main goal/hurdle: I'd like to have the power to say: "MyCharacter = equipWeapon *myCharacter* swordOfPower"
--However, this results in an infinite loop. I have a working functionality now, but it requires multiple variables. Messy. Example below.

--Need to set up test with HUnit, currently using, in GHCI:
--defaultStats = CharacterStats { con = 10, str = 10, dex = 10, int = 10, cha = 10, wis = 10,
--characterEquipment = EquippedEquipment { weapon = Nothing, armor = Nothing, accessory = Nothing } }

--swordOfPower = WeaponEquipment "Sword of Power" 0 5 0 0 0 0
--myNewStats = equipWeapon defaultStats swordOfPower

--myNewStats
-- (The result): CharacterStats {con = 10, str = 10, dex = 10, int = 10, cha = 10, wis = 10, characterEquipment = EquippedEquipment
-- {weapon = Just (Equipment {itemName = "Sword of Power", itemCon = 0, itemStr = 5, itemDex = 0, itemInt = 0, itemCha = 0, itemWis = 0}), armor = Nothing, accessory = Nothing}}

--I looked into the Lens library, but had troubles trying to install it as a dependency.
