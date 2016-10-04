module LibSpec where
import Protolude
import Test.Hspec

import Lib

spec :: Spec
spec = do
    describe "myString" $ do
        it "is great" $ do
            myString `shouldBe` "great"
