main = print ""

robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

getName robot = robot name

getAttack robot = robot attack

getHp robot = robot hp

setName robot newName = robot (\(_n, a, hp) -> (newName, a, hp))

setAttack robot newAttack = robot (\(n, _a, hp) -> (n, newAttack, hp))

setHp robot newHp = robot (\(n, a, _hp) -> (n, a, newHp))

printRobot robot = robot (\(n, a, hp) -> n ++ " attack: " ++ (show a) ++ " hp: " ++ (show hp))

damage robot dmg = robot (\(n, a, hp) -> robot (n, a, hp - dmg))

fight attacker defender = damage defender attack
  where
    attack =
      if getHp attacker > 10
        then getAttack attacker
        else 0

killerRobot = robot ("Kill3r", 25, 200)

nicerRobot = setName killerRobot "kitty"

gentlerRobot = setAttack killerRobot 5

softerRobot = setHP killerRobot 50

robots = [killerRobot, nicerRobot, gentlerRobot, softerRobot]

hps = map (\r -> getHp r) robots

fightAll robots robot = map (\r -> getHp (fight robot r)) robots
