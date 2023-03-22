module DNDTests where

import Test.HUnit
import Character
import Equipment


testSuite :: Test
testSuite = TestList [
    testEquipWeapon,
    testEquippingArmor
    ]

testEquipWeapon :: Test
testEquipWeapon = TestCase $ do
    let testCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing Nothing Nothing)
        testWeapon = WeaponEquipment "Sword of Power" 2 3 0 0 0 0
        equippedCharacter = equip testCharacter testWeapon
        expectedCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just (Equipment "Sword of Power" 2 3 0 0 0 0)) Nothing Nothing)
    expectedCharacter @?= equippedCharacter

testEquippingArmor :: Test
testEquippingArmor = TestCase $ do
    let testCharacter2 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing Nothing Nothing)
        testArmor = ArmorEquipment "Chain Mail" 2 0 (-1) 0 0 0
        equippedCharacter2 = equip testCharacter2 testArmor
        expectedCharacter2 = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing (Just (Equipment "Chain Mail" 2 0 (-1) 0 0 0)) Nothing)
    expectedCharacter2 @?= equippedCharacter2
