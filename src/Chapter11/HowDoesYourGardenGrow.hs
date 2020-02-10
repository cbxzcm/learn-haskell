module Chapter11.HowDoesYourGardenGrow where

-- 1. Given the type
data FlowerType =
    Gardenia |
    Daisy |
    Rose |
    Lilac
    deriving Show

type Gardener = String

data Garden =
    Garden Gardener FlowerType
    deriving Show

-- What is the sum of products normal form of Garden?

-- Answer:

data GardenerNormalForm =
    GardeniaType Gardener |
    DaisyType Gardener |
    RoseType Gardener |
    LilacType Gardener
