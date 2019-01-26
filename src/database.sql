-- C) Select Address (or group of addresses) with maximum even balance
--      This is the agreggated non-spent coins such that sum is even
SELECT *
FROM (
	SELECT coin_address,
	       CASE WHEN SUM(coin_amount) % 2 = 0 THEN SUM(coin_amount) ELSE 0 END as balance,
	       rank() OVER (ORDER BY CASE WHEN SUM(coin_amount) % 2 = 0 THEN SUM(coin_amount) ELSE 0 END DESC) as kind
	FROM coins
	WHERE coin_spender_tx_hash IS NULL
	GROUP BY coin_address) as T
WHERE kind = 1

-- D) Top 10 blocks that has spent more coins (by total value of coins)

SELECT * 
FROM (
	SELECT b.block_hash,
	       sum(c.coin_amount) as total_spent
	FROM blocks b,
	     transactions t,
	     coins c
	WHERE b.block_hash = t.tx_block_hash AND c.coin_spender_tx_hash = t.tx_hash	
	GROUP BY b.block_hash) as myname
ORDER BY total_spent DESC
LIMIT 10

--------------------------------------------------------------------------------


CREATE TABLE blocks (
  block_hash TEXT PRIMARY KEY,
  block_height INTEGER,
  block_time TIMESTAMP
)

CREATE TABLE transactions (
  tx_hash TEXT PRIMARY KEY,
  tx_block_hash TEXT REFERENCES blocks(block_hash)
)

CREATE TABLE coins {
  coin_address TEXT,
  coin_amount BIGINT,
  coin_creator_tx_hash TEXT REFERENCES transactions (tx_hash),
  coin_spender_tx_hash TEXT REFERENCES transactions (tx_hash)
}
