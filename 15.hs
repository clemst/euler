-- on a 20x20 grid, moves ending in the bottom right corner are a successing of 40 moves that can be either Down or Right.
-- to land on bottom right corner, there must be equal number of Down and Right moves, so 20 Downs and 20 Rights
-- this is therefore a matter of finding how many permutation exist on the list of 40 moves containing 20 down move and 20 right moves
-- basic combinatory math gives us: C(n,k) where n=40 and k=20

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

main = print $ choose 40 20
