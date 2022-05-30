{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Map (Map)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Control.Concurrent.STM (check)
import System.Random 
{-

--FEES 
Fees are 1% of every pot
upon fees distribution half of it goes to the contract owner, and half gets minted into Lotto coins and distributed 

-}




--functions below are made specifically for format and order above
--reads addres string
readAddr :: [[Char]]-> [Char]
readAddr p = p!!0
--allows conversion of numeric strings into doubles for maths calculations
textToDouble :: [Char] -> Double
textToDouble textNum = read textNum :: Double
--reads deposited amount as string (later converted using the function above)
readDepAmountTxt :: [[Char]]-> [Char]
readDepAmountTxt p = p!!1
--reads and converts to double deposit amount
getDepAmount :: [[Char]] -> Double
getDepAmount p = textToDouble (readDepAmountTxt p)
--reads the user provided guess word
readWord :: [[Char]]-> [Char]
readWord p = p!!2


--takes a list of submission that have inssufficient or overpaid amounts and generates a list of transactions to payback
getPaybackTransactionList :: [Submission] -> [[[Char]]]
getPaybackTransactionList submissionList =
    let
        li1 = map getSubmissionAddress submissionList
        li2 = map getSubmissionAmount submissionList
        pbTxL = combineIndex li1 li2
    in
        pbTxL

--data format for submissions in lottery
data Submission = Submission {payoutAddress :: [Char], amount :: Double, guessAttempt :: [Char] }deriving Show
getSubmissionAddress :: Submission -> [Char]
getSubmissionAddress submission = payoutAddress submission
getSubmissionAmount :: Submission -> [Char]
getSubmissionAmount submission = show (amount submission) :: String
getSubmissionAttempt :: Submission -> [Char]
getSubmissionAttempt submission = payoutAddress submission

--Conversion from the example data format to Submission list
--transaction list entry to -> submission 
textLtoSubm :: [[Char]] -> Submission
textLtoSubm p =
    let
        pAddr = readAddr p
        pDA = getDepAmount p
        pWrd = readWord p
        s = Submission pAddr pDA pWrd
    in
        s
--transaction list to submissiion list
aTx2SubmL::[[[Char]]] -> [Submission]
aTx2SubmL txL = map textLtoSubm txL

---





--filters transactions too low to buy a ticket
incompleteDeposits :: [Submission] -> [Submission]
incompleteDeposits x = filter isEnough x
    where
        isEnough (Submission addr dA wrd) = dA < tC
--filters transactions that got their tickets registered
completeDeposits :: [Submission] -> [Submission]
completeDeposits x = filter isEnough x
    where
        isEnough (Submission addr dA wrd) = dA >= tC

--function used to return amounts after the ticket cost is deducted
deduceTicketCost :: [Submission] -> [Submission]
deduceTicketCost s = map deduce s
    where
        deduce (Submission addr dA wrd) = Submission addr (dA - tC) wrd



--generates a list of wallets and amounts to return for either overpaid tickets or too low for a ticket
makePaybackList :: [Submission] -> [Submission]
makePaybackList sL = (incompleteDeposits sl)++deduceTicketCost (completeDeposits sl)

--datatype for a ticket entry
data Ticket = Ticket [Char] [Char] deriving Show


--making tickets from valid submissions
mintTickets :: [Submission] -> [Ticket]
mintTickets sL = map extract (completeDeposits sL)
    where
        extract (Submission addr dA wrd) = Ticket wrd addr



--checks checks if a given letter is an element of a given keyword
checkForCharInAWord :: Char -> [Char] -> Bool
checkForCharInAWord letter inKeyword = elem letter inKeyword



--add one to the provided value if the bool is true
incP :: Bool -> Int -> Int
incP doIncrement x =
    if doIncrement then x+1 else x

--take a word and make it of specific length, it needs to receive a word, a number of letters it currently has, a number of letters it's meant to get, 
--and a character to put in missing spaces in case there isn't enough letters 
makeWordNLetters :: [Char] -> Int -> Int -> Char -> [Char]
makeWordNLetters word currentNumLetters targetNumLetters fSW
    |currentNumLetters > targetNumLetters = take targetNumLetters word
    |currentNumLetters < targetNumLetters = word ++ take (targetNumLetters-currentNumLetters) [fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW, fSW]
    |currentNumLetters == targetNumLetters = word


--checks answer at 11 letters length, hardcoded into function
checkAnswer :: [Char] -> [Char] -> Int
checkAnswer wordAnswerSource wordAttemptSource =
    let
        wordAnswerCount = length wordAnswerSource
        wordAttemptCount = length wordAttemptSource
        wordAnswer = makeWordNLetters wordAnswerSource wordAnswerCount 10 '0'
        wordAttempt = makeWordNLetters wordAttemptSource wordAttemptCount 10 '1'

        l1 = wordAnswer!!0 == wordAttempt!!0
        l2 = wordAnswer!!1 == wordAttempt!!1
        l3 = wordAnswer!!2 == wordAttempt!!2
        l4 = wordAnswer!!3 == wordAttempt!!3
        l5 = wordAnswer!!4 == wordAttempt!!4
        l6 = wordAnswer!!5 == wordAttempt!!5
        l7 = wordAnswer!!6 == wordAttempt!!6
        l8 = wordAnswer!!7 == wordAttempt!!7
        l9 = wordAnswer!!8 == wordAttempt!!8
        l10 = wordAnswer!!9 == wordAttempt!!9
    in
        incP l9 (incP l8 (incP l7 (incP l6 (incP l5 (incP l4 (incP l3 (incP l2 (incP l1 0))))))))

--MAKES A WORD, NEED A RANDOMIZING FUNCTION
generateWord :: Int -> [Char]
generateWord numOfCharacters =
    let
        alphabet = ['A','B'..'Z']
        --need to create a randomizing function
        word = "ABCDEFGHIJ"
    in
        word

--reads a guess word from ticket data type

wordFromTicket :: Ticket -> [Char]
wordFromTicket ticket = extractWord ticket
    where
        extractWord (Ticket attempt address) = attempt


--checks if a given ticket's guess word matches given answer word
checkTicketWordMatch :: Ticket -> [Char] -> Int
checkTicketWordMatch ticket answer = points
    where
        word = wordFromTicket ticket
        points = checkAnswer answer word

--data type for tickets evaluated for matching letters in a word
data SignedTicket = SignedTicket {attempt :: [Char], address :: [Char], points :: Int} deriving Show
--evaluate tickets word to check how many letters match and sign it
signTicket :: Ticket -> SignedTicket
signTicket ticket = givePoints ticket
    where
        givePoints (Ticket attempt address) = SignedTicket attempt address (checkTicketWordMatch ticket kWrd)


--evaluate a whole bunch of tickets to check their points
evaluateSubmissions :: [Ticket] -> [SignedTicket]
evaluateSubmissions ticketList = map signTicket ticketList

--reads signed ticket points
readSignedTicketPoints :: SignedTicket -> Int
readSignedTicketPoints signedTicket = getPoints signedTicket
    where
        getPoints (SignedTicket attempt address points) = points

--sorts a lits of tickets by the number of points, from the most to the least points
sortSignedTicketsByPoints :: [SignedTicket] -> [SignedTicket]
sortSignedTicketsByPoints = sortBy (comparing points)


--takes all the tickets that have same amount of points as the best ticket
getTopSubmissions :: [SignedTicket] -> [SignedTicket]
getTopSubmissions sortedSignedTicketList =
    let
        firstTicket = head sortedSignedTicketList
        topScore = readSignedTicketPoints firstTicket
        --filter by custom datatype property points is a proeprty
        topScoreTickets = [t | t <- sortedSignedTicketList, points t == topScore]


    in
        topScoreTickets


--temporary function that will perofrm send operation
sendAmountToAddress :: [Char] -> Double -> Bool
sendAmountToAddress address amount =
    let
        success = True
    in
        success


--calculates payout to the current winners from the poll
getPayPerHead :: Int -> Double -> Double
getPayPerHead winnersCount totalWinnings =
    let
        winnersDouble = fromIntegral winnersCount :: Double
        payout = totalWinnings / winnersDouble
    in
        payout

--reads an address of the signed ticket
readSignedTicketAddress :: SignedTicket -> [Char]
readSignedTicketAddress winningTicket = address winningTicket

--zips two lists
combineIndex :: [a] -> [a] -> [[a]]
combineIndex pair1 pair2 = [ [a, b] | (a, b) <- zip pair1 pair2]

data QueuedTx = QueuedTx {txAddress :: [Char], payAmount :: Double} deriving Show
--makeTransaction :: [Char] -> Double -> [QueuedTx]
--makeTransaction address amount =  

--calculates how much money is in the poll
calculateThePot :: Int -> Double -> Double
calculateThePot numberOfSumbissions ticketPrice = pot
    where
        dbSub = fromIntegral numberOfSumbissions :: Double
        pot = dbSub * ticketPrice
--needs to send payouts somehow, or create transactions



--payout formula get the percentage of points achieved
-- pP = points/maxPoints*100%
-- payout = pP*pP*poll
getWinningAmount :: Double -> Int -> Int -> Double
getWinningAmount pollAmount points maxPoints =
    let
        dbP = fromIntegral points :: Double
        dbMp = fromIntegral maxPoints :: Double
        pP = dbP/dbMp
    in
        pP*pP*pollAmount






--TESTING --------------------------------------
--ticketCost
tC :: Double
tC = 2.0000 --ada


--DATA HANDLING EXAMPLE
--INCOMING TRANSACTIONS WITH EXTRATED WORD FROM TX DATA, DATA VALIDATION AND CLEANING ASSUMED COMPLETE
--format "wallet/payoutaddress, deposit amount, keyword"
--KEYWORD -> 5 NON REAPEATING LETTERS, THE ORDER OF THE LETTERS DOESN'T MATTER
p1 :: [[Char]]
p1 = ["xxxxxxxxxxx1", "2.53", "ABCDE"]
p2 :: [[Char]]
p2 = ["xxxxxxxxxxx2", "2.33", "MNZLD"]
p3 :: [[Char]]
p3 = ["xxxxxxxxxxx3", "2.00", "DCEWL"]
p4 :: [[Char]]
p4 = ["xxxxxxxxxxx4", "0.98", "CBPTD"]
p5 :: [[Char]]
p5 = ["xxxxxxxxxxx5", "2.01", "MONEY"]
p6 :: [[Char]]
p6 = ["xxxxxxxxxxx6", "0.30", "NICEO"]
p7 :: [[Char]]
p7 = ["111111walletWinner", "2.00", "ABVDEFGH"]
p9 :: [[Char]]
p9 = ["022021walletWinner", "2.00", "AXCDEFGH"]
p10 :: [[Char]]
p10 = ["020321walletWinner", "4.00", "ABCDEFGH"]
p11 :: [[Char]]
p11 = ["020241walletWinner", "3.00", "AXXDEFGH"]
p8 :: [[Char]]
p8 = ["020521walletWinner", "2.00", "AXXDEFGH"]


--combininhg transaction details into a a list
aTx :: [[[Char]]]
aTx = [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11]
poll :: Double
poll = calculateThePot (length aTx) tC


sl :: [Submission]
sl = aTx2SubmL aTx
pbl :: [Submission]
pbl = makePaybackList sl

pblTx :: [[[Char]]]
pblTx = getPaybackTransactionList  pbl
t :: [Ticket]
t = mintTickets sl
t0 :: Ticket
t0 = head t
--SEND PAYBACKS FROM THE LIST
--DRAW A WORD i.e.
kWrd :: [Char]
kWrd = generateWord 11




st :: [SignedTicket]
st = evaluateSubmissions t

sst :: [SignedTicket]
sst = reverse (sortSignedTicketsByPoints st)

winners :: [SignedTicket]
winners = getTopSubmissions sst


wadr :: [[Char]]
wadr = map readSignedTicketAddress winners
--top points
w1 :: SignedTicket
w1 = head sst
wP :: Int
wP = points w1
--max possible points
mP :: Int
mP = length kWrd
--calculate rewrds
rew :: Double
rew = getWinningAmount poll wP mP


--number of winners
wc :: Int
wc = length winners

pph :: Double
pph = getPayPerHead wc rew

txtPph :: String
txtPph = show pph


pL :: [String]
pL = take wc (repeat txtPph)


genTx :: [[[Char]]]
genTx = combineIndex wadr pL
--amount left for the next poll
nxP :: Double
nxP = poll - rew



{-
TO DO"
*RANDOMIZE WORD
*LOAD TRANSACTIONS FROM REAL BLOCKCHAIN

THEN
*FRONT END TO BUY SUBMISSIONS -> BETWEEN 1 AND N, WITH INPUT NAMES OR RANDOMLY GENERATED

-}
