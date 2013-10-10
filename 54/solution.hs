import qualified Data.List as L
import System.Environment

data Suit = Spades | Hearts | Diamonds | Clubs
          deriving(Eq, Ord, Bounded, Show)
                  
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen |King | Ace
           deriving(Eq, Ord, Bounded, Show)

data Card = Card { cVal :: Value, 
                   cSuit :: Suit }
          deriving(Eq)
                  
instance Ord Card where
  compare c1 c2 =
    compare (cVal c1) (cVal c2)
                  
instance Show Card where
  show c = show (cVal c) ++ " of " ++ show (cSuit c)

data Hand = Hand Card Card Card Card Card
          deriving(Eq, Show)

class ToCardList a where
  toCardList :: a -> [Card]

instance ToCardList Hand where
  toCardList h = case h of
    Hand one two three four five -> [one, two, three, four, five]
    
instance Ord Hand where
  compare = compareHands
  
data Rank = HighCard Card 
          | OnePair Card 
          | TwoPair Card Card 
          | ThreeOfAKind Card 
          | Straight Card 
          | Flush Card 
          | FullHouse Card Card 
          | FourOfAKind Card 
          | StraightFlush Card 
          | RoyalFlush Card
          deriving(Eq, Show)
                  
instance Ord Rank where
  compare r1 r2 = comped
    where
      comped = 
        case r1 of
          HighCard c ->
            case r2 of
              HighCard c' -> compare c c'
              _ -> LT
          OnePair c ->
            case r2 of
              HighCard _ -> GT
              OnePair c' -> compare c c'
              _ -> LT
          TwoPair c1 c2 ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair c1' c2' -> if c1 == c1'
                             then compare c2 c2'
                             else compare c1 c1'
              _ -> LT
                   
          ThreeOfAKind c->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind c' -> compare c c'
              _ -> LT

          Straight c ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind _ -> GT
              Straight c' -> compare c c'
              _ -> LT

          Flush c ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind _ -> GT
              Straight _ -> GT
              Flush c' -> compare c c'
              _ -> LT

          FullHouse c3 c2 ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind _ -> GT
              Straight _ -> GT
              Flush _ -> GT
              FullHouse c3' c2' ->
                if c3 == c3'
                then compare c2 c2'
                else compare c3 c3'
              _ -> LT

          FourOfAKind c ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind _ -> GT
              Straight _ -> GT
              Flush _ -> GT
              FullHouse _ _ -> GT
              FourOfAKind c' -> compare c c'
              _ -> LT
          
          StraightFlush c ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind _ -> GT
              Straight _ -> GT
              Flush _ -> GT
              FullHouse _ _ -> GT
              FourOfAKind _ -> GT
              StraightFlush c' -> compare c c'          
              _ -> LT
          
          RoyalFlush c ->
            case r2 of
              HighCard _ -> GT
              OnePair _ -> GT
              TwoPair _ _ -> GT
              ThreeOfAKind _ -> GT
              Flush _ -> GT
              FullHouse _ _ -> GT
              Straight _ -> GT
              FourOfAKind _ -> GT
              StraightFlush _ -> GT
              RoyalFlush c' -> compare c c'



main = do
  [fileName] <- getArgs
  fileString <- readFile fileName
  let lined = lines fileString
  let firstWinner = L.foldl' parseLineToWinner 0 lined
  
  putStrLn $ show firstWinner
  
  

parseLineToWinner :: Int -> String -> Int
parseLineToWinner acc line = 
  if hand1 > hand2
  then acc + 1
  else acc
  where
    [h11, h12, h13, h14, h15, h21, h22, h23, h24, h25] = map parseWord $ words line
    hand1 = Hand h11 h12 h13 h14 h15
    hand2 = Hand h21 h22 h23 h24 h25
    

parseWord :: String -> Card
parseWord [c, s] = Card val suit
  where
    val
      | c == '2' = Two
      | c == '3' = Three
      | c == '4' = Four
      | c == '5' = Five
      | c == '6' = Six
      | c == '7' = Seven
      | c == '8' = Eight
      | c == '9' = Nine
      | c == 'T' = Ten
      | c == 'J' = Jack
      | c == 'Q' = Queen
      | c == 'K' = King
      | c == 'A' = Ace
      | otherwise = error "parseError parseWord Value"
    suit
      | s == 'C' = Clubs
      | s == 'D' = Diamonds
      | s == 'S' = Spades
      | s == 'H' = Hearts
      | otherwise = error "parseError parseWord Suit"
parseWord _ = error "parseError parseWord"






-- Does not compare suits. Two flushes are always equal, decided with high card.
compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 =
  if comped /= EQ
  then comped
  else compare high1 high2
  where    
    comped = compare rank1 rank2
    rank1 = getRank hand1
    rank2 = getRank hand2    
    high1 = L.sortBy (flip compare) $ toCardList hand1
    high2 = L.sortBy (flip compare) $ toCardList hand2
    
getRank :: Hand -> Rank
getRank h =
  case royalFlush h of
    Just c -> RoyalFlush c
    _ -> case straightFlush h of
      Just c -> StraightFlush c
      _ -> case fourOfAKind h of
        Just c -> FourOfAKind c
        _ -> case fullHouse h of
          Just (c, c') -> FullHouse c c'
          _ -> case flush h of 
            Just c -> Flush c
            _ -> case straight h of
              Just c -> Straight c
              _ -> case threeOfAKind h of
                Just c -> ThreeOfAKind c
                _ -> case twoPair h of
                  Just (c, c') -> TwoPair c c'
                  _ -> case onePair h of
                    Just c -> OnePair c
                    _ -> let hc = highCard h
                         in HighCard hc

royalFlush :: Hand -> Maybe Card
royalFlush h
  | null cheque = L.find (\c -> cVal c == Ace) cardList
  | otherwise = Nothing
  where
    cardList@[one, two, three, four, five] = toCardList h
    (_, cheque) = L.foldl' (\(suit, remaining) card -> 
                             let suit' = cSuit card
                                 val = cVal card
                             in if suit' == suit &&
                                   elem val remaining
                                then (suit, L.delete val remaining)
                                else (suit, remaining)
                           ) (cSuit one, [Ten, Jack, Queen, King, Ace]) cardList

straightFlush :: Hand -> Maybe Card
straightFlush h =
  case flush h of
    Just c -> case straight h of
      Just c' -> Just c'
      _ -> Nothing
    _ -> Nothing

                  
flush :: Hand -> Maybe Card
flush h
  | length sameSuit == 5 = Just $ highCard h
  | otherwise = Nothing
  where
    cardList@[one, two, three, four, five] = toCardList h
    oneSuit = cSuit one
    sameSuit = L.filter (\c -> cSuit c == oneSuit) cardList
    
straight :: Hand -> Maybe Card
straight h
  | isStraight = Just highest
  | otherwise = Nothing
                 
  where
    cards = L.sortBy (flip compare) $ toCardList h

    (isStraight, highest) = L.foldl' (\(check, pCard) nCard ->
                                       let pVal = cVal pCard
                                           nVal = cVal nCard
                                       in if check &&
                                             nextValue pVal == nVal
                                          then (True, nCard)
                                          else (False, pCard)
                                     ) (True, head cards) $ tail cards
                 
    nextValue :: Value -> Value
    nextValue Ace = King
    nextValue King = Queen
    nextValue Queen = Jack
    nextValue Jack = Ten
    nextValue Ten = Nine
    nextValue Nine = Eight
    nextValue Eight = Seven
    nextValue Seven = Six
    nextValue Six = Five
    nextValue Five = Four
    nextValue Four = Three
    nextValue Three = Two
    nextValue Two = Ace

fourOfAKind :: Hand -> Maybe Card
fourOfAKind h
  | length l1 == 4 = Just one
  | length l2 == 4 = Just two
  | otherwise = Nothing
  where
    cardList@[one, two, three, four, five] = toCardList h
    oneVal = cVal one
    twoVal = cVal two
    
    l1 = filter (\c -> cVal c == oneVal) cardList
    l2 = filter (\c -> cVal c == twoVal) cardList
    
threeOfAKind :: Hand -> Maybe Card
threeOfAKind h
  | length l1 == 3 = Just one
  | length l2 == 3 = Just two
  | length l3 == 3 = Just three
  | otherwise = Nothing
  where
    cardList@[one, two, three, four, five] = toCardList h
    oneVal = cVal one
    twoVal = cVal two
    threeVal = cVal three
    
    l1 = filter (\c -> cVal c == oneVal) cardList
    l2 = filter (\c -> cVal c == twoVal) cardList
    l3 = filter (\c -> cVal c == threeVal) cardList
    
onePair :: Hand -> Maybe Card
onePair h
  | length l1 == 2 = Just one
  | length l2 == 2 = Just two
  | length l3 == 2 = Just three
  | length l4 == 2 = Just four
  | otherwise = Nothing
  where
    cardList@[one, two, three, four, five] = toCardList h
    oneVal = cVal one
    twoVal = cVal two
    threeVal = cVal three
    fourVal = cVal four
    
    l1 = filter (\c -> cVal c == oneVal) cardList
    l2 = filter (\c -> cVal c == twoVal) cardList
    l3 = filter (\c -> cVal c == threeVal) cardList
    l4 = filter (\c -> cVal c == fourVal) cardList


-- only returns something for two pairs, not full house
twoPair :: Hand -> Maybe (Card, Card)
twoPair h
  | length pairs == 2 = Just (pairs' !! 1, pairs' !! 0)
  | otherwise = Nothing
  where
    cardList@[one, two, three, four, five] = toCardList h
    pairs' = L.sort pairs
    pairs = map (\(v,c) -> Card v c) $
            L.foldl' (\(pairsFound) card ->
                       let val = cVal card
                       in if lookup val pairsFound == Nothing &&
                          length (filter (\c -> cVal c == val) cardList) >= 2
                          then (val, cSuit card):pairsFound
                          else pairsFound
                     ) [] $ cardList

fullHouse :: Hand -> Maybe (Card, Card)
fullHouse h =
  case threeOfAKind h of 
    Just c -> case onePair h of
      Just c' -> Just (c, c')
      _ -> Nothing
    _ -> Nothing

highCard :: Hand -> Card
highCard h = L.foldl1' max $ toCardList h