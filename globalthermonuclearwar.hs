module GlobalThermonuclearWar where

--Game Design:
--You have territory
--You have troops (land, water, air)
--You have nukes
--You get points for how much territory you have after 20 turns
--Every Turn you can send troops to attack the enemy, they fight the enemy's troops and depending on who wins territory switches hands
--During your turn you can also send nukes, they reduce the territory the enemy has available and also their troops (but not their nukes)

data PlayerID = P1 | P2 deriving (Eq, Show)
data Victory = P1Won | P2Won | Stalemate | NotEnded deriving (Eq, Show)

data Player = Player {
    curentTroops :: Troops Int,
    activeTroops :: Troops Int,
    getNukes     :: Int,
    getTerritory :: Int
    } deriving (Show,Eq)

data Troop = Land | Water | Air deriving (Show,Eq)
data Troops a = Troops {
    land :: a,
    water :: a,
    air :: a
    } deriving (Show,Eq)

instance Functor Troops where
    fmap f (Troops x y z) = Troops (f x) (f y) (f z)

playerStartTroops    = Troops 200 200 200
playerStartNukes     = 10
playerStartTerritory = 10000


emptyTroops = (Troops 0 0 0)
makeStandardPlayer = Player playerStartTroops emptyTroops playerStartNukes playerStartTerritory

transferTerritory amount source@(Player _ _ _ t1) target@(Player _ _ _ t2) = (modifyTerritory source (-amount'), modifyTerritory target amount')
                    where amount' = if (t1 < amount) then t1 else if (t2 < -amount) then -t2 else amount

modifyTerritory (Player c a n t) i
    | i+t < 0   = Player c a n 0
    | otherwise = Player c a n (i+t)

nukePlayer nukes player@(Player c a n t) = modifyTerritory (Player survivingTroops survivingAttackers n t) (-destroyedTerritory)
                where survivingAttackerFraction = 0.5 ^ nukes
                      survivingAttackers = fmap (ceiling . (*) survivingAttackerFraction . fromIntegral) a
                      survivingTroopsFraction = 0.9 ^ nukes
                      survivingTroops = fmap (ceiling . (*) survivingTroopsFraction . fromIntegral) c
                      destroyedTerritory = nukes * 1000

troopZip f (Troops a0 b0 c0) (Troops a1 b1 c1) = (Troops (f a0 a1) (f b0 b1) (f c0 c1))

prepareNukes sn (Player c a n t) = (Player c a n' t, sn')
                            where sn' = min sn n
                                  n'  = n - sn'

sendTroop troops (Player c a n t) = Player c' a' n t
                            where troops' = troopZip min troops c
                                  c' = troopZip (-) c troops'
                                  a' = troopZip (+) a troops'

endCombat survivors (Player c a n t) = Player c' emptyTroops n t
                            where c' = troopZip (+) c survivors


--fight mechanics:
--squads of 10 units
--need full squad to count
--first 3 air squads fight
--1. air squad vs land squad results in 3:8 losses
--2. air squad vs air squad results in 5:5 losses
--3. air squad vs water squad results in 6:2 losses
--now first 3 land squads fight
--1. land squad vs water squad results in 3:8 losses
--2. land squad vs land squad results in 5:5 losses
--3. land squad vs air squad results in 6:2 losses
--now first 3 water squads fight
--1. water squad vs air squad results in 3:8 losses
--2. water squad vs water squad results in 5:5 losses
--3. water squad vs land squad results in 6:2 losses
--surviving units retreat
--whoever has the most units left alive gets land. to the tune of unitsuperioritycount * 10

landAdd (Troops l w a) n = Troops (l+n) w a
waterAdd (Troops l w a) n = Troops l (w+n) a
airAdd (Troops l w a) n = Troops l w (a+n)

land' = (land,landAdd)
water' = (water,waterAdd)
air' = (air,airAdd)


fight :: Troops Int -> Troops Int -> (Troops Int,Troops Int,Int)
fight troops1 troops2 = (t1''',t2''',calcTerritoryGain t1''' t2''')
            where t1Air = air troops1
                  t1Land = land troops1
                  t2Air = air troops2
                  t2Land = land troops2
                  (t1',t2') = combatRound air' (land',air',water') troops1 troops2
                  (t1'',t2'') =  combatRound land' (water',land',air') t1' t2'
                  (t1''',t2''') =  combatRound water' (air',water',land') t1'' t2''


combatRound att (d0,d1,d2) t1 t2 = (t1''',t2''')
        where   (t1',t2') = combat att d0 t1 t2 combatCounter
                (t1'',t2'') = combat att d1 t1' t2' combatEven
                (t1''',t2''') = combat att d2 t1'' t2'' combatBad

combat :: (Troops Int -> Int, Troops Int -> Int -> Troops Int) -> (Troops Int -> Int, Troops Int -> Int -> Troops Int) -> Troops Int -> Troops Int ->  (Int,Int) -> (Troops Int, Troops Int)
combat m1 m2 t1 t2 kind = (snd m2 (snd m1 t1 m1t1') m2t1',snd m2 (snd m1 t2 m1t2') m2t2')
                where   helper k1 k2 = if (k1 > 10 && k2 > 10) then kind else (0,0)
                        (m1t1', m2t2') = helper (fst m1 t1) (fst m2 t2)
                        (m1t2', m2t1') = helper (fst m1 t2) (fst m2 t1)

combatCounter = (-3,-8)
combatEven = (-5,-5)
combatBad = (-6,-2)
calcTerritoryGain (Troops l1 w1 a1) (Troops l2 w2 a2) = ((m l2+m w2+m a2)-(m l1+m w1+m a1))*10
                                        where m x = min x 50


{-
--preserved because it causes the AI not to play :D
fight :: Troops Int -> Troops Int -> (Troops Int,Troops Int,Int)
fight troops1 troops2 = (troops1,troops2,calcTerritoryGain troops1 troops2)

combatCounter t1 t2 = (t1-3,t2-8)
combatEven t1 t2 = (t1-5,t2-5)
combatBad t1 t2 = (t1-6,t2-2)
calcTerritoryGain (Troops l1 w1 a1) (Troops l2 w2 a2) = ((l1+w1+a1)-(l2+w2+a2))*10
-}

playGame p1Controller p2Controller (p1,p2) turnTimer = res
    where (p1',p2',tT') = gameRound p1Controller p2Controller (p1,p2) turnTimer
          res           = if tT' < 20 then
                            playGame p1Controller p2Controller (p1',p2') tT'
                          else
                            (getTerritory p1', getTerritory p2')

playGameWithIO p1Controller p2Controller (p1,p2) turnTimer = do
       print $ "Turn "++(show turnTimer)
       (p1',p2',tT') <- gameRoundWithIO p1Controller p2Controller (p1,p2) turnTimer 
       if tT' < 20 then
            playGameWithIO p1Controller p2Controller (p1',p2') tT'
          else
            return (getTerritory p1', getTerritory p2')

gameRoundWithIO :: Num t => (Player -> t -> IO (Troops Int, Int)) -> (Player -> t -> IO (Troops Int, Int)) -> (Player,Player) -> t -> IO (Player, Player, t)
gameRoundWithIO p1ControllerActions p2ControllerActions (p1,p2) turnTimer = do
    (p1t,p1n) <- p1ControllerActions p1 turnTimer
    (p2t,p2n) <- p2ControllerActions p2 turnTimer
    let p1' = sendTroop p1t p1
    let p2' = sendTroop p2t p2
    troopPrint 1 p1'
    troopPrint 2 p2'
    let p1n'@(_,p1nukes) = prepareNukes p1n p1'
    let p2n'@(_,p2nukes) = prepareNukes p2n p2'
    nukePrint 1 p1nukes
    nukePrint 2 p2nukes
    let (p1'',p2'') = gameCalculations p1n' p2n'
    return (p1'',p2'',turnTimer')
    where   turnTimer' = turnTimer + 1
            troopPrint n p = print $ "Player "++(show n)++" has send the following Troops: " ++ (show . activeTroops $ p)
            nukePrint n pn = print $ "Player "++(show n)++" has launched "++(show pn)++" nukes"

gameRound p1ControllerActions p2ControllerActions (p1,p2) turnTimer = (p1'',p2'',turnTimer')
    where   (p1t,p1n) = p1ControllerActions p1 turnTimer
            (p2t,p2n) = p2ControllerActions p2 turnTimer
            p1' = sendTroop p1t p1
            p2' = sendTroop p2t p2
            p1n' = prepareNukes p1n p1'
            p2n' = prepareNukes p2n p2'
            (p1'',p2'') = gameCalculations p1n' p2n'
            turnTimer' = turnTimer + 1

gameCalculations (p1,p1n) (p2,p2n) = (p1''',p2''')
    where   p1' = nukePlayer p2n p1
            p2' = nukePlayer p1n p2
            (p1t,p2t,territory) = fight (activeTroops p1') (activeTroops p2')
            p1'' = endCombat p1t p1'
            p2'' = endCombat p2t p2'
            (p1''',p2''') = transferTerritory territory p1'' p2''


getPlayerActions p turnTimer = do
    t <- getTroopSending p
    pn <- getNukeSending p
    return (t,pn)

getTroopSending :: Player -> IO (Troops Int)
getTroopSending x = do
    print x
    print "Commander, declare which Troops to send to battle!"
    print "Land: "
    l <- readLn
    print "Water: "
    w <- readLn
    print "Air: "
    a <- readLn
    return (Troops l w a)

getNukeSending :: Player -> IO Int
getNukeSending x = do
    print x
    print "Commander, do you want to nuke the enemy?"
    n <- readLn
    return n