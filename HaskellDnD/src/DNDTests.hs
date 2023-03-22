module DNDTests where

import Test.HUnit
import Character
import Equipment


testSuite :: Test
testSuite = TestList [
    testEquipWeapon
    ]

testEquipWeapon :: Test
testEquipWeapon = TestCase $ do
    let testCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment Nothing Nothing Nothing)
        testWeapon = WeaponEquipment "Sword of Power" 2 3 0 0 0 0
        equippedCharacter = equip testCharacter testWeapon
        expectedCharacter = CharacterStats 10 10 10 10 10 10 (EquippedEquipment (Just (Equipment "Sword of Power" 2 3 0 0 0 0)) Nothing Nothing)
    expectedCharacter @?= equippedCharacter
