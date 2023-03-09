module Main (main) where
import Character
import Test.HUnit

--Need to set up test with HUnit, currently using:
-- let swordOfPower = Equipment { itemName = "nothing", itemCon = 0, itemStr = 10, itemDex = 0, itemInt = 0, itemCha = 0, itemWis = 0 }
-- let mystats = equipItem defaultStats swordOfPower
-- myStats (prints result)
defaultStats = CharacterStats { con = 10, str = 10, dex = 10, int = 10, cha = 10, wis = 10, characterEquipment = EquippedEquipment { weapon = Nothing, armor = Nothing, accessory = Nothing } }

main :: IO ()
main = do
  return ()

