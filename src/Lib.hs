module Lib
    ( Blockchain (..),
      Block (..),
      Transaction (..),
      Coin (..),
      Output (..),
      coinbaseTx,
      coins,
      mineBlock,
      getCoinbase,
      ancestors,
      maxVolume
    ) where

import           Data.Int
import           Data.Word
import           Data.Ord
import           Data.Set                       (toList, fromList)
import           Data.List
import qualified Data.Map as M

-----------------------------------------------------------------------------
-- Data types

type Blockchain = [Block]

data Coin = Coin {
  transaction :: Transaction,
  vout        :: Int
} deriving (Show, Eq)

data Block = Block {
  time         :: Int64,
  transactions :: [Transaction]
} deriving (Show, Eq)

data Transaction = Transaction {
  inputs   :: [Coin],
  outputs  :: [Output]
} deriving (Show, Eq)

data Output = Output {
  address :: String,
  value   :: Word64
} deriving (Show, Eq)

instance Ord Output where
  compare = comparing value <> comparing address

instance Ord Coin where
  compare = comparing transaction <> comparing vout

-- I would need a txid of some kind
instance Ord Transaction where
  compare = comparing inputs <> comparing outputs
-----------------------------------------------------------------------------
-- Exercises
-- A) given two timestamps and a blockchain, return the (address, value) 
--    where value is the max volume in that timeline
maxVolume :: Int64 -> Int64 -> Blockchain -> Maybe (String, Word64)
maxVolume ini fin blockchain = toPair <$> maxOut 
  where
    -- select relevant blocks
    inBoundBlock b = (time b >= ini) && (time b <= fin)
    -- Output -> (Address, Int)
    toPair o = (address o, value o)
    -- group outputs by address
    sortAndGroup outs = M.fromListWith (++) [ (k, [v]) | (k, v) <- outs ]

    subChain = filter inBoundBlock blockchain
    txs = concatMap transactions subChain
    outs = concatMap outputs txs
    pairs = map toPair outs
    uOuts = map (uncurry Output) $ M.toList $ M.map sum $ sortAndGroup pairs
    maxOut = headMay $ sortOn Down uOuts


-- B) return all coins from all the coinbase transactions
ancestors :: Coin -> [Coin]
ancestors coin =
  if null creatorInputs -- isCoinbase 
  then coins creatorTx
  else makeUnique $ concatMap ancestors creatorInputs
  where
    creatorTx = transaction coin
    creatorInputs = inputs creatorTx

-----------------------------------------------------------------------------
-- Helpers to construct my altcoin:
-- The goal is to make impossible the impossible and make hard 
-- to create inconsistent blockchains
-- TODO: Add tx validation (Sum inputs == Sum outputs)

coins :: Transaction -> [Coin]
coins tx = map (Coin tx) [0 .. length (outputs tx) - 1]

coinbaseTx :: String -> Transaction
coinbaseTx address = Transaction [] [Output address 50]

mineBlock :: String -> Blockchain -> [Transaction] -> Blockchain
mineBlock address []         _   = [Block 1 [coinbaseTx address]]
mineBlock address blockchain txs = newBlock : blockchain
 where
  newTime  = (time $ head blockchain) + 1
  newBlock = Block newTime (coinbaseTx address : txs)

getCoinbase :: Int -> Blockchain -> Maybe Transaction
getCoinbase bh bc = _getCoinbase bh (reverse bc)

_getCoinbase :: Int -> Blockchain -> Maybe Transaction
_getCoinbase 0      _          = Nothing
_getCoinbase _      []         = Nothing
_getCoinbase 1      blockchain = Just $ head $ transactions $ head blockchain
_getCoinbase blockH blockchain = _getCoinbase (blockH - 1) (tail blockchain)

headMay :: [a] -> Maybe a
headMay []     = Nothing
headMay (x:xs) = Just x

makeUnique :: Ord a => [a] -> [a]
makeUnique = toList . fromList
