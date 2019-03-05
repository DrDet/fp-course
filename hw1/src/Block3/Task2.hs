module Block3.Task2
  (
    Town(..)
  , Castle(..)
  , Church(..)
  , Library(..)
  , makeHouse -- smart constructor
  , buildCastle
  , buildSpecialBuilding
  , buildHouse
  , enterLord
  , buildTownWalls
  ) where

import           Data.List.NonEmpty (NonEmpty (..), toList, (<|))

data Town = Town
  { castle          :: Maybe Castle
  , specialBuilding :: Maybe (Either Church Library)
  , houses          :: NonEmpty House
  } deriving Show

data Castle = Castle
  { hasLord  :: Bool
  , hasWalls :: Bool
  } deriving Show

data Church = Church
  deriving Show

data Library = Library
  deriving Show

newtype House = House { residentsCnt :: Int }
  deriving Show

makeHouse :: Int -> House
makeHouse n
  | n > 0 && n < 5 = House n
  | otherwise      = error "Incorrect amount of house residents"

buildCastle :: Town -> Maybe Town
buildCastle town = case town of
  Town{ castle = Nothing } -> Just town{ castle = Just (Castle False False) }
  _                        -> Nothing

buildSpecialBuilding :: Either Church Library -> Town -> Maybe Town
buildSpecialBuilding building town = case town of
  Town{ specialBuilding = Nothing } -> Just town'
    where
      town' = town{ specialBuilding = Just building }
  _                                 -> Nothing

buildHouse :: Int -> Town -> Either Town String
buildHouse n town
  | n > 0 && n < 5 = Left town'
  | otherwise      =
      Right "Couldn't build the house: a house requires from 1 to 4 residents"
  where
    town' = town{ houses = House n <| houses town }

enterLord :: Town -> Either Town String
enterLord Town{ castle = Nothing } =
  Right "Lord was unable to enter your town: there's no castle"
enterLord Town{ castle = Just Castle{ hasLord = True } } =
  Right "Lord was unable to enter your town: another lord is already inside"
enterLord town@Town{ castle = Just justCastle } = Left town'
  where town'   = town{ castle = Just castle' }
        castle' = justCastle{ hasLord = True }

buildTownWalls :: Town -> Either Town String
buildTownWalls Town{ castle = Nothing } =
  Right "Couldn't build town walls: there's no castle"
buildTownWalls Town{ castle = Just Castle{ hasLord = False }} =
  Right "Couldn't build town walls: there's no lord in your castle"
buildTownWalls town@Town{ castle = Just justCastle }
  | hasWalls justCastle      =
      Right "Couldn't build town walls: it's already built"
  | population < 10 =
      Right "Couldn't build town walls: you need to have at least 10 citizens"
  | otherwise       = Left town{ castle = Just castle' }
    where
      castle'    = justCastle{ hasWalls = True }
      population = foldr ((+) . residentsCnt) 0 (toList (houses town))
