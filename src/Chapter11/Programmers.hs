module Chapter11.Programmers where

data OperatingSystem =
    GnuPlusLinux |
    OpenBSDPlusNevermindJustBSDStill |
    Mac |
    Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell |
    Agda |
    Idris |
    PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer {
        os :: OperatingSystem,
        lang :: ProgLang
    }
    deriving (Eq, Show)

-- Write a function that generates all possible values of Programmer. Use the provided
-- lists of inhabitants of OperatingSystem and ProgLang.

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Answer:
allProgrammers :: [Programmer]
allProgrammers =
    [ Programmer { os = otherOs, lang = otherLang }
    | otherOs   <- allOperatingSystems
    , otherLang <- allLanguages
    ]


