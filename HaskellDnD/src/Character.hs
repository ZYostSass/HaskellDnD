{-# LANGUAGE RankNTypes #-}

--RankN allows me to take an argument that is a function. VERY useful for equipping.
module Character where
import Test.HUnit
import Equipment
import Data.Maybe (Maybe(..))

-- import Test.HUnit
-- The baseline character stat templace, which includes equipment with stats on them.
data CharacterStats = CharacterStats
    {
        charCon :: Int,
        charStr :: Int,
        charDex :: Int,
        charInt :: Int,
        charCha :: Int,
        charWis :: Int,
        characterEquipment :: EquippedEquipment
    } deriving (Show)


-- Equipped items can currently be either a weapon, armor or accessory.
data EquippedEquipment = EquippedEquipment
    {
        weapon :: Maybe Equipment,
        armor :: Maybe Equipment,
        accessory :: Maybe Equipment
    } deriving (Show)

instance Eq Equipment where
  (==) (Equipment name1 con1 str1 dex1 int1 cha1 wis1) (Equipment name2 con2 str2 dex2 int2 cha2 wis2) =
    name1 == name2 &&
    con1 == con2 &&
    str1 == str2 &&
    dex1 == dex2 &&
    int1 == int2 &&
    cha1 == cha2 &&
    wis1 == wis2

instance Eq CharacterStats where
  (CharacterStats c1 s1 d1 i1 ch1 w1 eq1) == (CharacterStats c2 s2 d2 i2 ch2 w2 eq2) =
    c1 == c2 && s1 == s2 && d1 == d2 && i1 == i2 && ch1 == ch2 && w1 == w2 && eq1' == eq2'
      where eq1' = (weapon eq1, armor eq1, accessory eq1)
            eq2' = (weapon eq2, armor eq2, accessory eq2)


equip :: (EquipmentType a) => CharacterStats -> a -> CharacterStats
equip character equipment =
    let equippedEquipment = characterEquipment character
    in character { characterEquipment = case equipment of
        weapon -> equippedEquipment { weapon = Just (toEquipment weapon) }
        armor -> equippedEquipment { armor = Just (toEquipment armor) }
        accessory -> equippedEquipment { accessory = Just (toEquipment accessory) }
    }

toEquipment :: EquipmentType a => a -> Equipment
toEquipment equipment = Equipment
    { itemName = name equipment,
      itemCon = con equipment,
      itemStr = str equipment,
      itemDex = dex equipment,
      itemInt = int equipment,
      itemCha = cha equipment,
      itemWis = wis equipment
    }

-- The following equip functions allow for equipping a weapon, armor or accessory.
-- And will also add their stat bonuses to the character. IE, a +5 str sword of power
-- Would return "15 str" for a base template character that has 10 str.
{-equipWeapon :: CharacterStats -> WeaponEquipment -> CharacterStats
equipWeapon character weapon = 
  let newEquipment = WeaponEquipment 
                        { itemName = wepName 
                        , itemCon = wepCon 
                        , itemStr = wepStr 
                        , itemDex = wepDex 
                        , itemInt = wepInt 
                        , itemCha = wepCha 
                        , itemWis = wepWis
                        }

equipArmor :: CharacterStats -> ArmorEquipment -> CharacterStats
equipArmor character armor =
    let equipped = characterEquipment character
        newEquipment = EquippedEquipment 
            { weapon = weapon equipped,
              armor = Just $ Equipment 
                { 
                    itemName = armName armor,
                    itemCon = armCon armor,
                    itemStr = armStr armor,
                    itemDex = armDex armor,
                    itemInt = armInt armor,
                    itemCha = armCha armor,
                    itemWis = armWis armor
                },
              accessory = accessory equipped
            }
    in character { characterEquipment = newEquipment }

equipAccessory :: CharacterStats -> AccessoryEquipment -> CharacterStats
equipAccessory character accessory =
    let equipped = characterEquipment character
        newEquipment = EquippedEquipment 
            { weapon = weapon equipped,
              armor = armor equipped,
              accessory = Just $ Equipment 
                { 
                    itemName = accName accessory,
                    itemCon = accCon accessory,
                    itemStr = accStr accessory,
                    itemDex = accDex accessory,
                    itemInt = accInt accessory,
                    itemCha = accCha accessory,
                    itemWis = accWis accessory
                }
            }
    in character { characterEquipment = newEquipment }

--Allows for comparison of CharacterStats for testing, possibly future functionalities.

-- Early attempt at HUnit tests, but had difficulties importing it?
-- Keeping for reminder to self for automated testing going forward.

testEquipItem :: Test
testEquipItem = TestList
    [ "Equipping a weapon should increase character stats" ~:
        equipItem character weaponItem ~?= expectedWeaponCharacter
    , "Equipping armor should increase character stats" ~:
        equipItem character armorItem ~?= expectedArmorCharacter
    , "Equipping an accessory should increase character stats" ~:
        equipItem character accessoryItem ~?= expectedAccessoryCharacter
    ]
    where 
        character = CharacterStats 10 10 10 10 10 10 emptyEquippedEquipment
        weaponItem = Equipment "Sword" 0 2 0 0 0 0
        armorItem = Equipment "Chainmail" 2 0 1 0 0 0
        accessoryItem = Equipment "Ring of Protection" 0 0 0 0 2 0
        emptyEquippedEquipment = EquippedEquipment Nothing Nothing Nothing
        expectedWeaponCharacter = CharacterStats 10 12 10 10 10 10 (EquippedEquipment (Just weaponItem) Nothing Nothing)
        expectedArmorCharacter = CharacterStats 12 10 11 10 10 10 (EquippedEquipment Nothing (Just armorItem) Nothing)
        expectedAccessoryCharacter = CharacterStats 10 10 10 10 12 10 (EquippedEquipment Nothing Nothing (Just accessoryItem))
-}