module Equipment where

--For future use, all equipment will have a name and potentially multiple stats applied to it.
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

--So a player may equip a weapon. Previously had
-- "weapon" as the name to equip later; however, this lead to trouble
-- when trying to have unique names like "Sword of Power".
-- Breaking up multiple equipment types allows more modularity in the future.
data WeaponEquipment = WeaponEquipment
    {
        wepName :: String,
        wepCon :: Int,
        wepStr :: Int,
        wepDex :: Int,
        wepInt :: Int,
        wepCha :: Int,
        wepWis :: Int
    } deriving (Show)

data ArmorEquipment = ArmorEquipment
    {
        armName :: String,
        armCon :: Int,
        armStr :: Int,
        armDex :: Int,
        armInt :: Int,
        armCha :: Int,
        armWis :: Int
    } deriving (Show)

data AccessoryEquipment = AccessoryEquipment
    {
        accName :: String,
        accCon :: Int,
        accStr :: Int,
        accDex :: Int,
        accInt :: Int,
        accCha :: Int,
        accWis :: Int
    } deriving (Show)  

class EquipmentType a where
    name :: a -> String
    con :: a -> Int
    str :: a -> Int
    dex :: a -> Int
    int :: a -> Int
    cha :: a -> Int
    wis :: a -> Int



instance EquipmentType WeaponEquipment where
    name = wepName
    con = wepCon
    str = wepStr
    dex = wepDex
    int = wepInt
    cha = wepCha
    wis = wepWis

instance EquipmentType ArmorEquipment where
    name = armName
    con = armCon
    str = armStr
    dex = armDex
    int = armInt
    cha = armCha
    wis = armWis
    
instance EquipmentType AccessoryEquipment where
    name = accName
    con = accCon
    str = accStr
    dex = accDex
    int = accInt
    cha = accCha
    wis = accWis