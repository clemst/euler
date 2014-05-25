import Data.Digits

lastdigits = unDigits 10 . reverse . take 10 . digitsRev 10

wSum 1 = 1
wSum n = wSum (n-1) + lastdigits n^n

main = print $ lastdigits $ wSum 1000
