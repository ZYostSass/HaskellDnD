module DNDTests where

import Test.HUnit
import Equipment
import Dice


testSuite :: Test
testSuite = TestList [
    testEquipWeapon,
    testEquippingArmor,
    testChainRoll
    ]

testEquipWeapon :: Test
testEquipWeapon = TestCase $ do
    let testCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing Nothing Nothing)
        swordOfPower = WeaponEquipment "Sword of Power" 0 2 0 1 0 1
        equippedCharacter = equip swordOfPower testCharacter
        expectedCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just (WeaponType swordOfPower)) Nothing Nothing)
    expectedCharacter @=? equippedCharacter

testEquippingArmor :: Test
testEquippingArmor = TestCase $ do
    let testCharacter2 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing Nothing Nothing)
        testArmor = ArmorEquipment "Chain Mail" 2 0 (-1) 0 0 0
        equippedCharacter2 = equip testArmor testCharacter2
        expectedCharacter2 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing (Just (ArmorType testArmor)) Nothing)
    expectedCharacter2 @=? equippedCharacter2

testChainRoll :: Test
testChainRoll = TestCase $ do
  let dice1 = Dice (\n -> (4, n+1))
      dice2 = Dice (\n -> (5, n+1))
      dice3 = Dice (\n -> (6, n+1))
      chain = do
        x <- dice1
        y <- dice2
        z <- dice3
        return (x + y + z)
      (result, _) = roll chain 0
  result @=? 15