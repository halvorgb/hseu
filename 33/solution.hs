maxNum = 99
minNum = 11

solution :: [(Int, Int)]
solution = [(i,j) | i <- numGen, j <- numGen, i < j, commonDigit i j, naiveCancellable i j]
  where
    numGen = [n | n <- [minNum..maxNum], mod n 10 /= 0]
    digits n = (div n 10, mod n 10)
    commonDigit n m =
      let (nTen, nOne) = digits n
          (mTen, mOne) = digits m
      in (nTen == mTen || nTen == mOne || nOne == mTen || nOne == mOne)
         
    naiveCancellable n m
      | nTen == nOne = if nTen == mTen
                       then cheque nOne mOne
                       else cheque nTen mTen
      | mTen == mOne = if nTen == mTen --dobbel ja
                       then cheque nOne mOne
                       else cheque nTen mTen
      | nTen == mOne &&
        nOne == mTen = cheque nTen mTen || cheque nOne mOne
      | nTen == mTen = cheque nOne mOne
      | nOne == mOne = cheque nTen mTen
      | nTen == mOne = cheque nOne mTen
      | nOne == mTen = cheque nTen mOne
      where
        cheque d d' =
          fromIntegral d / fromIntegral d' == res
        res = fromIntegral n / fromIntegral m
        (nTen, nOne) = digits n
        (mTen, mOne) = digits m
      