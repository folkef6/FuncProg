import Cards
import RunGame


sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            ,1 + 1 + size []
            ,1 + 1 + 0
            , 2]
            
            

--displayCard converts a Card (i.e Card (Numeric 5) Hearts) to its correspondent string (i.e 5 of hearts)
displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++ " of " ++ show suit
displayCard (Card rank suit) = show rank ++ " of " ++ show suit


--display function (Shows the cards in a given hand as strings): 

display :: Hand -> String
display [] = "Hand is empty, error in display" 
display (card:[]) = displayCard card
display (card:hand) = displayCard card ++ ", " ++ display hand

--cardValue calculates the value of a single card 
cardValue :: Card -> Int
cardValue (Card (Numeric n) _) = n
cardValue (Card Ace _) = 11
cardValue (Card rank _) = 10

--numberOfAces keeps track of the ammount of aces in the hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:hand) | rank card == Ace = 1 + numberOfAces hand
                         | rank card /= Ace = 0 + numberOfAces hand

--initialValue function calculates the value of a hand without reducing the value of aces in case of the player going bust (over 21)
initialValue :: Hand -> Int
initialValue [] = 0
initialValue (card:hand) = cardValue card + initialValue hand

--value function calculates the value of the hand with aces being reduced to 1 if nescessary. 
 
value :: Hand -> Int 
value (card:hand) | initialValue(card:hand) <= 21 =  initialValue(card:hand)
                  | initialValue(card:hand) > 21 = initialValue(card:hand) - 10*(numberOfAces(card:hand))
                  
--gameOver checks if a given hand has gone bust

gameOver :: Hand -> Bool
gameOver hand = value hand > 21 

--winner checks the guest's and the banks hands and determines the winner. 

winner :: Hand -> Hand -> Player 
winner guest bank | value guest > value bank = Guest 
                  | value guest < value bank = Bank 
                  | value guest == value bank = Bank 


--Task B: 

-- To create a full deck I take the cartesian product of two lists containing respectivly all possible rank and suits.

createRankList :: [Rank]
createRankList = ([Numeric x | x <- [2..10] ]) ++ [Jack,Queen,King,Ace]

suitList = [Hearts,Spades,Diamonds,Clubs]

fullDeck :: Deck
fullDeck   = [Card rank suit | rank <- createRankList , suit <- suitList] 

--prop_size_fullDeck checks if the deck has 52 cards

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

--Draw function:
draw :: Deck->Hand -> (Deck, Hand) 
draw deck hand = undefined 

first :: (a, b) -> (a,b)
first (x, y) = (x,y)







