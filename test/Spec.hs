import           Test.Hspec
import           Data.Maybe
import           Lib

-------------------------------------------------------------------
-- MOCKED BLOCKCHAIN

-- Block 1: Genesis Block
blockchain1 = mineBlock "cb_addr1" [] []
c1 = fromJust $ getCoinbase 1 blockchain1 -- first coinbase tx

-- Block 2
t1 = Transaction (coins c1) [Output "addr1" 10, Output "addr2" 40]
blockchain2 = mineBlock "cb_addr2" blockchain1 [t1]
c2 = fromJust $ getCoinbase 2 blockchain2

-- Block 3
t2 = Transaction (coins c2) [Output "addr2" 20, Output "addr3" 30]
t3 = Transaction [head $ coins t1] [Output "addr1" 10]


t4 = Transaction ((coins t2) <> (tail $ coins t1))
                 [Output "addr4" 20, Output "addr3" 70]

blockchain3 = mineBlock "cb_addr3" blockchain2 [t2, t3, t4]
c3 = fromJust $ getCoinbase 3 blockchain3

-- Block 4
t5 = Transaction (coins t2) [Output "addr3" 50]
t6 = Transaction (coins t4) [Output "addr3" 90]

blockchain4 = mineBlock "cb_addr4" blockchain3 [t5, t6]
c4 = fromJust $ getCoinbase 4 blockchain4

----------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "A) MaxVolume" $ do
    it "handle timestamps resulting with empty chains" $
       maxVolume (-1) (-2) blockchain4 `shouldBe` Nothing
    it "Genesis block max volume should be first address and first coin" $
      maxVolume 1 1 blockchain4 `shouldBe` Just ("cb_addr1", 50)
    it "max volume should not include all the outputs from all the chain" $
      maxVolume 4 4 blockchain4 `shouldBe` Just ("addr3", 140)
    it "max volume for the full chain should include all the outputs" $
      maxVolume 1 4 blockchain4 `shouldBe` Just ("addr3", 240)

  describe "B) Ancestors" $ do
    it "Ancestors of a coinbase should be itself" $ do
      let coinbase1 = head $ coins c1 
      ancestors coinbase1 `shouldBe` [coinbase1]
    it "Ancestors should find all of them and unique" $ do
      let coin = head $ coins t6
      let coinbase1 = head $ coins c1
      let coinbase2 = head $ coins c2
      ancestors coin `shouldBe` [coinbase1, coinbase2]
