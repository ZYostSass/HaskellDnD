module DNDTests where

import Test.HUnit
import Equipment
import Dice


testSuite :: Test
testSuite = TestList [
    testEquipWeapon,
    testReplaceWeapon,
    testEquippingArmor,
    --testEquipAllEquipmentTypes
    testChainRoll,
    testD4,
    testD6,
    testD8,
    testD20,
    testTotalStatCountGroup,
    testTotalStatCountSingle

    ]

testEquipWeapon :: Test
testEquipWeapon = TestCase $ do
    let testCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Nothing, Nothing, Nothing))
        swordOfPower = WeaponEquipment "Sword of Power" 0 2 0 1 0 1 2
        equippedCharacter = equip swordOfPower testCharacter
        expectedCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just swordOfPower, Nothing, Nothing))
    --putStrLn $ "Equipped character:\n" ++ show equippedCharacter
    --putStrLn $ "Expected character:\n" ++ show expectedCharacter
    expectedCharacter @=? equippedCharacter


--Writing this test revealed I can't update an existing character.
--Example: "testCharacter = equip swordOfBiggerPower testCharacter."
--Next goal would be to write an 'update character' function to do this, if possible.
testReplaceWeapon :: Test
testReplaceWeapon = TestCase $ do
    let swordOfPower = WeaponEquipment "Sword of Power" 0 2 0 1 0 1 2
        testCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just swordOfPower, Nothing, Nothing))
        swordOfBiggerPower = WeaponEquipment "Sword of Bigger Power" 0 3 0 2 0 4 6
        equippedCharacter = equip swordOfBiggerPower testCharacter
        expectedCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just swordOfBiggerPower, Nothing, Nothing))
    --putStrLn $ "Equipped character:\n" ++ show equippedCharacter
    --putStrLn $ "Expected character:\n" ++ show expectedCharacter
    expectedCharacter @=? equippedCharacter

testEquippingArmor :: Test
testEquippingArmor = TestCase $ do
    let testCharacter2 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Nothing, Nothing, Nothing))
        testArmor = ArmorEquipment "Chain Mail" 2 0 (-1) 0 0 0 5
        equippedCharacter2 = equip testArmor testCharacter2
        expectedCharacter2 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Nothing, Just testArmor, Nothing))
    --putStrLn  $ "Equipped Char:\n" ++ show equippedCharacter2
    --putStrLn $ "Expected character:\n" ++ show expectedCharacter2
    expectedCharacter2 @=? equippedCharacter2


{-testEquipAllEquipmentTypes :: Test
testEquipAllEquipmentTypes = TestCase $ do
    let testCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Nothing, Nothing, Nothing))
        swordOfPower = WeaponEquipment "Sword of Power" 0 2 0 1 0 1 2
        plateArmor = ArmorEquipment "Plate Armor" 2 0 0 0 2 0 1
        ringOfPower = AccessoryEquipment "Ring of Power" 0 0 0 0 2 2
        equippedCharacter = equip swordOfPower testCharacter
        equippedCharacter2 = equip plateArmor equippedCharacter
        equippedCharacter3 = equip ringOfPower equippedCharacter2
        expectedCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just swordOfPower, Just plateArmor, Just ringOfPower))
    putStrLn $ "Equipped character:\n" ++ show equippedCharacter
    putStrLn $ "Expected character:\n" ++ show expectedCharacter
    expectedCharacter @=? equippedCharacter3-}


--Initial dice implementation before factoring in sides.
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


--For dice tests, I was assured it was 'random' (seed-based) because each dice
--Is not rolling the same value for d4, d6, d8, d20 with the same seed.
testD4 :: Test
testD4 = TestCase $ do
   -- use a fixed seed; had to figure out how to test. Seed 1 result is 2.
  let seed = 1
      (result, _) = roll d4 seed
  --putStrLn $ "Rolled:" ++ show result
  result @=? 2

testD6 :: Test
testD6 = TestCase $ do
   -- use a fixed seed; had to figure out how to test. Seed 1 result is 6.
  let seed = 1
      (result, _) = roll d6 seed
  result @=? 6

testD8 :: Test
testD8 = TestCase $ do
   -- use a fixed seed; had to figure out how to test. Seed 1 result is 6.
  let seed = 1
      (result, _) = roll d8 seed
  result @=? 6

testD20 :: Test
testD20 = TestCase $ do
   -- use a fixed seed; had to figure out how to test. Seed 1 result is 3.
  let seed = 1
      (result, _) = roll d20 seed
  result @=? 3

-- Make sure it works with one character first.
testTotalStatCountSingle :: Test
testTotalStatCountSingle = TestCase $ do
  let stats1 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Nothing, Nothing, Nothing))
      actual = totalStatCount [stats1]
      expected = 60
  expected @=? actual

-- Does it work with a full party?
testTotalStatCountGroup :: Test
testTotalStatCountGroup = TestCase $ do
  let stats1 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Nothing, Nothing, Nothing))
      stats2 = CharacterStats 1 2 3 4 5 6 (EquippedEquipment (Nothing, Nothing, Nothing))
      stats3 = CharacterStats 20 20 20 20 20 20 (EquippedEquipment (Nothing, Nothing, Nothing))
      characterList = [stats1, stats2, stats3]
      expected = 201
      actual = totalStatCount characterList
  expected @=? actual