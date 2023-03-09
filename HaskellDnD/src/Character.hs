module Character where

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

-- The baseline equipment template, factoring in potential stat boosts. Early version, simple.
data Equipment = Equipment
    { 
        itemName :: String,
        itemCon :: Int,
        itemStr :: Int,
        itemDex :: Int,
        itemInt :: Int,
        itemCha :: Int,
        itemWis :: Int
    } deriving (Show)

data EquippedEquipment = EquippedEquipment
    {
        weapon :: Maybe Equipment,
        armor :: Maybe Equipment,
        accessory :: Maybe Equipment
    } deriving (Show)

defaultStats = CharacterStats { con = 10, str = 10, dex = 10, int = 10, cha = 10, wis = 10, characterEquipment = EquippedEquipment { weapon = Nothing, armor = Nothing, accessory = Nothing } }

equipItem :: CharacterStats -> Equipment -> CharacterStats
equipItem character item = 
    let equipped = characterEquipment character
        newItemName = itemName item
        newCon = itemCon item + con character
        newStr = itemStr item + str character
        newDex = itemDex item + dex character
        newInt = itemInt item + int character
        newCha = itemCha item + cha character
        newWis = itemWis item + wis character
        newEquipment = case newItemName of
            "weapon" -> EquippedEquipment (Just item) (armor equipped) (accessory equipped)
            "armor" -> EquippedEquipment (weapon equipped) (Just item) (accessory equipped)
            "accessory" -> EquippedEquipment (weapon equipped) (armor equipped) (Just item)
            _ -> equipped
    in character { characterEquipment = newEquipment, con = newCon, str = newStr, dex = newDex, int = newInt, cha = newCha, wis = newWis }


{-testEquipItem :: Test
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