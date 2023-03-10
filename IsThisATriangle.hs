import Data.List (sort, sortBy)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = let [x, y, z] = sortBy (flip compare) [a, b, c] in x < y + z

{-
spec :: Spec
spec = do
  describe "isTriangle" $ do
    it "works for some examples" $ do
      isTriangle 1 2 2 `shouldBe` True
      isTriangle 7 2 2 `shouldBe` False
      isTriangle 1 3 2 `shouldBe` False
      isTriangle 3 1 2 `shouldBe` False
      isTriangle 1 2 3 `shouldBe` False
      isTriangle 5 1 2 `shouldBe` False
      isTriangle 1 2 5 `shouldBe` False
      isTriangle 2 5 1 `shouldBe` False
      isTriangle 4 2 3 `shouldBe` True
      isTriangle 2 2 2 `shouldBe` True
      isTriangle 5 1 5 `shouldBe` True
      isTriangle (-1) 2 3 `shouldBe` False
      isTriangle 1 (-2) 3 `shouldBe` False
      isTriangle 1 2 (-3) `shouldBe` False
      isTriangle 0 2 3 `shouldBe` False
      -}