--The equipment module focuses on stat and equipment stats.
--As well as utility functions such as defining Eq.
module Equipment(
    CharacterStats(..),
    EquippedEquipment(..),
    WeaponEquipment(..),
    ArmorEquipment(..),
    AccessoryEquipment(..),
    totalStatCount,
    equip
) where 

--Started with character stats as a baseline; future would be a character file.
--With things like race, names etc.
data CharacterStats a = CharacterStats
    {
        charCon :: Int,
        charStr :: Int,
        charDex :: Int,
        charInt :: Int,
        charCha :: Int,
        charWis :: Int,
        -- Equipment a character may or may not have.
        equippedEquipment :: EquippedEquipment a
    } deriving (Show)

--Had an early excuse to test a foldable implementation on my data.
--Some players like to gauge the strength of their character based on stats from dice rolls.
--By implementing as [a], can make a list of characters as a bonus / excuse for foldable practice.
totalStatCount :: [CharacterStats a] -> Int
totalStatCount = foldr (\stats acc -> charCon stats + charStr stats + charDex stats + charInt stats + charCha stats + charWis stats + acc) 0

data EquippedEquipment a = EquippedEquipment (Maybe a, Maybe a, Maybe a) deriving (Eq, Show)

--Old version of equippedEquipment. 

--The data type for equipment, which can be a weapon, armor or accessory.
{-data EquippedEquipment a = EquippedEquipment
    {
        weapon :: Maybe (EquipmentType a WeaponEquipment),
        armor :: Maybe (EquipmentType a ArmorEquipment),
        accessory :: Maybe (EquipmentType a AccessoryEquipment)
    } deriving (Eq, Show) -- -}

--Just a nice way to print out the weapon/armor/accessory values for testing.
--Could be implemented into the game too.
instance (Show a) => Show (EquipmentType a b) where
       show (WeaponType e) = "Weapon: " ++ show e
       show (ArmorType e) = "Armor: " ++ show e
       show (AccessoryType e) = "Accessory: " ++ show e

--The base equipment class that factors in all potential members.
--This is a good example of class types; it defines the data BUT ALSO
--How functions can work with this base class.  Specifically, equipmentType + equipment.
class (Show a, Eq a) => Equipment a where
       -- A name, like 'Sword of Power'
       itemName :: a -> String
       itemCon :: a -> Int
       itemStr :: a -> Int
       itemDex :: a -> Int
       itemInt :: a -> Int
       itemCha :: a -> Int
       itemWis :: a -> Int
       --To string, to represent what type of equipment it is; armor/wep etc.
       equipmentType :: a -> String
       --The specific equipment type, and the data it contains. IE, 'weapon' and its stats.
       equipment :: a -> EquipmentType a b
       -- Not all items may have damage/ac; but then, not all items have stats.
       itemDamage :: a -> Int
       itemAC :: a -> Int

data EquipmentType a b = WeaponType a | ArmorType a | AccessoryType a

type Weapon = EquipmentType WeaponEquipment
type Armor = EquipmentType ArmorEquipment
type Accessory = EquipmentType AccessoryEquipment

data WeaponEquipment = WeaponEquipment
    {
        wepName :: String,
        wepCon :: Int,
        wepStr :: Int,
        wepDex :: Int,
        wepInt :: Int,
        wepCha :: Int,
        wepWis :: Int,
        wepDamage :: Int
    } deriving (Eq, Show)

instance Equipment WeaponEquipment where
       itemName = wepName
       itemCon = wepCon
       itemStr = wepStr
       itemDex = wepDex
       itemInt = wepInt
       itemCha = wepCha
       itemWis = wepWis
       equipmentType _ = "Weapon"


data ArmorEquipment = ArmorEquipment
    {
        armName :: String,
        armCon :: Int,
        armStr :: Int,
        armDex :: Int,
        armInt :: Int,
        armCha :: Int,
        armWis :: Int,
        armAC :: Int
    } deriving (Eq, Show)

instance Equipment ArmorEquipment where
       itemName = armName
       itemCon = armCon
       itemStr = armStr
       itemDex = armDex
       itemInt = armInt
       itemCha = armCha
       itemWis = armWis
       equipmentType _ = "Armor"

data AccessoryEquipment = AccessoryEquipment
    {
        accName :: String,
        accCon :: Int,
        accStr :: Int,
        accDex :: Int,
        accInt :: Int,
        accCha :: Int,
        accWis :: Int
    } deriving (Eq, Show)  

instance Equipment AccessoryEquipment where
       itemName = accName
       itemCon = accCon
       itemStr = accStr
       itemDex = accDex
       itemInt = accInt
       itemCha = accCha
       itemWis = accWis
       equipmentType _ = "Accessory"

--While able to derive Eq for Wep, Arm, Acc, need to define Eq for Equip and CharStats.
--We are comparing the characters stats, AS WELL as their equipped item's stats.
instance (Equipment a, Eq a) => Eq (CharacterStats a) where
    (CharacterStats con1 str1 dex1 int1 cha1 wis1 equipped1) == (CharacterStats con2 str2 dex2 int2 cha2 wis2 equipped2) =
        con1 == con2 && str1 == str2 && dex1 == dex2 && int1 == int2 && cha1 == cha2 && wis1 == wis2 && equipped1 == equipped2

--Used to compare two Equipment types. Checks their values.
--If they don't match, then we return False.
instance (Equipment a, Eq a) => Eq (EquipmentType a b) where
  (WeaponType a) == (WeaponType b) = a == b
  (ArmorType a) == (ArmorType b) = a == b
  (AccessoryType a) == (AccessoryType b) = a == b
  _ == _ = False

--Spent the bulk of my time trying to make a readable, 'polymorphic' equip.
--I reworked this 4 or 5 times with major overhauls to my code, and spent the most time here...
--But am happy with my result! I can call 'character equip [equipmenttype]'! 
--But discovered can't equip more than 1 item...

--Takes a type Equipment of any 'a', and character of any 'a'.
-- The first component of the tuple is for weapons, the second is for armor, and the third is for accessories.
-- If an equipment slot is empty, it is represented by `Nothing`.

equip :: (Equipment a) => a -> CharacterStats a -> CharacterStats a
equip equipment character =
    case equipment of
        e -> case equipmentType e of
            "Weapon" -> character { equippedEquipment = updateEquipment (Just e, armor, accessory) }
            "Armor" -> character { equippedEquipment = updateEquipment (weapon, Just e, accessory) }
            "Accessory" -> character { equippedEquipment = updateEquipment (weapon, armor, Just e) }
            _ -> character
    where
        (weapon, armor, accessory) = getEquipmentTuple (equippedEquipment character)
        -- Update the equipped equipment tuple with the new equipment, ensuring that at most one item of each type is equipped.
        updateEquipment (newWeapon, newArmor, newAccessory)
            | newWeapon /= Nothing && weapon /= Nothing = EquippedEquipment (newWeapon, armor, accessory)
            | newArmor /= Nothing && armor /= Nothing = EquippedEquipment (weapon, newArmor, accessory)
            | newAccessory /= Nothing && accessory /= Nothing = EquippedEquipment (weapon, armor, newAccessory)
            | otherwise = EquippedEquipment (newWeapon, newArmor, newAccessory)
        -- Convert the equipped equipment tuple into a triple.
        getEquipmentTuple (EquippedEquipment (w, a, ac)) = (w, a, ac)

--Old, simpler equip before I implemented equipment as tuples.

{-
equip :: (Equipment a) => a -> CharacterStats a -> CharacterStats a
--Order of the function: 'equip SwordOfPower myCharacter'
equip equipment character =
    --The 'polymorphic' aspect. I redesigned my data to contain equipmentTypes.
    --Of 'Weapon' 'Armor' or 'Accessory' this is an iteration on earlier designs.
    case equipment of
        e -> case equipmentType e of
            "Weapon" -> character { equippedEquipment = (equippedEquipment character) { weapon = Just (WeaponType e) } }
            "Armor" -> character { equippedEquipment = (equippedEquipment character) { armor = Just (ArmorType e) } }
            "Accessory" -> character { equippedEquipment = (equippedEquipment character) { accessory = Just (AccessoryType e) } }
            --Just in case, if nothing is recognizes and equip is called with something, rather than 'break' the character,
            --Just return the original character argument.
            _ -> character}
-}