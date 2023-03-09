module Character where

import Equipment
import Data.Maybe (Maybe(..))

-- import Test.HUnit
-- The baseline character stat templace, which includes equipment with stats on them.
data CharacterStats = CharacterStats
    {
        con :: Int,
        str :: Int,
        dex :: Int,
        int :: Int,
        cha :: Int,
        wis :: Int,
        characterEquipment :: EquippedEquipment
    } deriving (Show)


-- Equipped items can currently be either a weapon, armor or accessory.
data EquippedEquipment = EquippedEquipment
    {
        weapon :: Maybe Equipment,
        armor :: Maybe Equipment,
        accessory :: Maybe Equipment
    } deriving (Show)

-- The following equip functions allow for equipping a weapon, armor or accessory.
-- And will also add their stat bonuses to the character. IE, a +5 str sword of power
-- Would return "15 str" for a base template character that has 10 str.
equipWeapon :: CharacterStats -> WeaponEquipment -> CharacterStats
equipWeapon character weapon = 
    let equipped = characterEquipment character
        newEquipment = EquippedEquipment 
            { weapon = Just $ Equipment 
                { 
                    itemName = wepName weapon,
                    itemCon = wepCon weapon,
                    itemStr = wepStr weapon,
                    itemDex = wepDex weapon,
                    itemInt = wepInt weapon,
                    itemCha = wepCha weapon,
                    itemWis = wepWis weapon
                },
              armor = armor equipped,
              accessory = accessory equipped
            }
    in character { characterEquipment = newEquipment }

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

{-
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