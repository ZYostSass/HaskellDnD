module Combat where

import Dice
import Equipment

{-attack :: (Equipment a) => CharacterStats a -> a -> CharacterStats a -> Dice a -> IO (CharacterStats a, Bool)
attack attackerEquip attacker defender attackDice = do
    let wep = case equippedEquipment attackerEquip of
                  EquippedEquipment _ (Just (WeaponType e)) _ -> Just e
                  _ -> Nothing
        wepDamageVal = case wep of
                          Just e -> fromIntegral $ itemDamage e
                          Nothing -> 0
        attackRoll = fst $ roll attackDice 1
        totalAttack = attackRoll + wepDamageVal
        defAC = case equippedEquipment defender of
                   EquippedEquipment (Just (ArmorType e)) _ _ -> itemAC e
                   _ -> defender
        success = totalAttack >= defAC
        newDefenderCon = if success then charCon defender - (attackRoll + wepDamageVal - defAC) else charCon defender
        newDefender = defender { charCon = newDefenderCon }
    return (newDefender, success)-}