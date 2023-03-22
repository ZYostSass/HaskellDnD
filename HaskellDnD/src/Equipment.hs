module Equipment where

data CharacterStats a = CharacterStats
    {
        charCon :: Int,
        charStr :: Int,
        charDex :: Int,
        charInt :: Int,
        charCha :: Int,
        charWis :: Int,
        equippedEquipment :: EquippedEquipment a
    } deriving (Show)

data EquippedEquipment a = EquippedEquipment
    {
        weapon :: Maybe (EquipmentType a WeaponEquipment),
        armor :: Maybe (EquipmentType a ArmorEquipment),
        accessory :: Maybe (EquipmentType a AccessoryEquipment)
    } deriving (Eq, Show)

instance (Show a) => Show (EquipmentType a b) where
       show (WeaponType e) = "Weapon: " ++ show e
       show (ArmorType e) = "Armor: " ++ show e
       show (AccessoryType e) = "Accessory: " ++ show e

class Equipment a where
       itemName :: a -> String
       itemCon :: a -> Int
       itemStr :: a -> Int
       itemDex :: a -> Int
       itemInt :: a -> Int
       itemCha :: a -> Int
       itemWis :: a -> Int
       equipmentType :: a -> String
       equipment :: a -> EquipmentType a b

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
        wepWis :: Int
    } deriving (Show)

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
        armWis :: Int
    } deriving (Show)

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
    } deriving (Show)  

instance Equipment AccessoryEquipment where
       itemName = accName
       itemCon = accCon
       itemStr = accStr
       itemDex = accDex
       itemInt = accInt
       itemCha = accCha
       itemWis = accWis
       equipmentType _ = "Accessory"

instance Eq WeaponEquipment where
    (WeaponEquipment name1 con1 str1 dex1 int1 cha1 wis1) == (WeaponEquipment name2 con2 str2 dex2 int2 cha2 wis2) =
        name1 == name2 && con1 == con2 && str1 == str2 && dex1 == dex2 && int1 == int2 && cha1 == cha2 && wis1 == wis2

instance Eq ArmorEquipment where
    (ArmorEquipment name1 con1 str1 dex1 int1 cha1 wis1) == (ArmorEquipment name2 con2 str2 dex2 int2 cha2 wis2) =
        name1 == name2 && con1 == con2 && str1 == str2 && dex1 == dex2 && int1 == int2 && cha1 == cha2 && wis1 == wis2

instance Eq AccessoryEquipment where
    (AccessoryEquipment name1 con1 str1 dex1 int1 cha1 wis1) == (AccessoryEquipment name2 con2 str2 dex2 int2 cha2 wis2) =
        name1 == name2 && con1 == con2 && str1 == str2 && dex1 == dex2 && int1 == int2 && cha1 == cha2 && wis1 == wis2


instance (Equipment a, Eq a) => Eq (CharacterStats a) where
    (CharacterStats con1 str1 dex1 int1 cha1 wis1 equipped1) == (CharacterStats con2 str2 dex2 int2 cha2 wis2 equipped2) =
        con1 == con2 && str1 == str2 && dex1 == dex2 && int1 == int2 && cha1 == cha2 && wis1 == wis2 && equipped1 == equipped2

instance (Equipment a, Eq a) => Eq (EquipmentType a b) where
  (WeaponType a) == (WeaponType b) = a == b
  (ArmorType a) == (ArmorType b) = a == b
  (AccessoryType a) == (AccessoryType b) = a == b
  _ == _ = False

equip :: (Equipment a) => a -> CharacterStats a -> CharacterStats a
equip equipment character =
    case equipment of
        e -> case equipmentType e of
            "Weapon" -> character { equippedEquipment = (equippedEquipment character) { weapon = Just (WeaponType e) } }
            "Armor" -> character { equippedEquipment = (equippedEquipment character) { armor = Just (ArmorType e) } }
            "Accessory" -> character { equippedEquipment = (equippedEquipment character) { accessory = Just (AccessoryType e) } }
            _ -> character