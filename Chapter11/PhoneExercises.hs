module PhoneExercises where
import           Data.Char
import           Data.List
import           Data.Maybe

type Digit = Char
type Presses = Int
data Value =
    A |
    B |
    C |
    D |
    E |
    F |
    G |
    H |
    I |
    J |
    K |
    L |
    M |
    N |
    O |
    P |
    Q |
    R |
    S |
    T |
    U |
    V |
    W |
    X |
    Y |
    Z |
    ONE |
    TWO |
    THREE |
    FOUR |
    FIVE |
    SIX |
    SEVEN |
    EIGHT |
    NINE |
    ZERO |
    ASTERISK |
    POUND |
    UPPERCASE |
    PLUS |
    SPACE |
    PERIOD |
    COMMA
    deriving (Show, Eq)

toMaybeValues :: Char -> Maybe [Value]
toMaybeValues c = case c of
    'A'       -> Just [UPPERCASE, A]
    'B'       -> Just [UPPERCASE, B]
    'C'       -> Just [UPPERCASE, C]
    'D'       -> Just [UPPERCASE, D]
    'E'       -> Just [UPPERCASE, E]
    'F'       -> Just [UPPERCASE, F]
    'G'       -> Just [UPPERCASE, G]
    'H'       -> Just [UPPERCASE, H]
    'I'       -> Just [UPPERCASE, I]
    'J'       -> Just [UPPERCASE, J]
    'K'       -> Just [UPPERCASE, K]
    'L'       -> Just [UPPERCASE, L]
    'M'       -> Just [UPPERCASE, M]
    'N'       -> Just [UPPERCASE, N]
    'O'       -> Just [UPPERCASE, O]
    'P'       -> Just [UPPERCASE, P]
    'Q'       -> Just [UPPERCASE, Q]
    'R'       -> Just [UPPERCASE, R]
    'S'       -> Just [UPPERCASE, S]
    'T'       -> Just [UPPERCASE, T]
    'U'       -> Just [UPPERCASE, U]
    'V'       -> Just [UPPERCASE, V]
    'W'       -> Just [UPPERCASE, W]
    'X'       -> Just [UPPERCASE, X]
    'Y'       -> Just [UPPERCASE, Y]
    'Z'       -> Just [UPPERCASE, Z]
    'a'       -> Just [A]
    'b'       -> Just [B]
    'c'       -> Just [C]
    'd'       -> Just [D]
    'e'       -> Just [E]
    'f'       -> Just [F]
    'g'       -> Just [G]
    'h'       -> Just [H]
    'i'       -> Just [I]
    'j'       -> Just [J]
    'k'       -> Just [K]
    'l'       -> Just [L]
    'm'       -> Just [M]
    'n'       -> Just [N]
    'o'       -> Just [O]
    'p'       -> Just [P]
    'q'       -> Just [Q]
    'r'       -> Just [R]
    's'       -> Just [S]
    't'       -> Just [T]
    'u'       -> Just [U]
    'v'       -> Just [V]
    'w'       -> Just [W]
    'x'       -> Just [X]
    'y'       -> Just [Y]
    'z'       -> Just [Z]
    '1'       -> Just [ONE]
    '2'       -> Just [TWO]
    '3'       -> Just [THREE]
    '4'       -> Just [FOUR]
    '5'       -> Just [FIVE]
    '6'       -> Just [SIX]
    '7'       -> Just [SEVEN]
    '8'       -> Just [EIGHT]
    '9'       -> Just [NINE]
    '0'       -> Just [ZERO]
    '+'       -> Just [PLUS]
    ' '       -> Just [SPACE]
    '.'       -> Just [PERIOD]
    ','       -> Just [COMMA]
    otherwise -> Nothing

data Button =
    Button {
        name :: Digit,
        values :: [Value]
    } deriving Show
data DaPhone = DaPhone [Button] deriving Show

phone = DaPhone
    [ Button { name = '1', values = [ONE] }
    , Button { name = '2', values = [A, B, C, TWO] }
    , Button { name = '3', values = [D, E, F, THREE] }
    , Button { name = '4', values = [G, H, I, FOUR] }
    , Button { name = '5', values = [J, K, L, FIVE] }
    , Button { name = '6', values = [M, N, O, SIX] }
    , Button { name = '7', values = [P, Q, R, S, SEVEN] }
    , Button { name = '8', values = [T, U, V, EIGHT] }
    , Button { name = '9', values = [W, X, Y, Z, NINE] }
    , Button { name = '0', values = [PLUS, SPACE, ZERO] }
    , Button { name = '*', values = [UPPERCASE, ASTERISK] }
    , Button { name = '#', values = [PERIOD, COMMA, POUND] }
    ]

convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U ist haha"
    , "Lol ok. Have U ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"
    ]

findButtonsMaybe :: [Button] -> [Value] -> [Maybe (Digit, Presses)]
findButtonsMaybe buttons inputValues =
    fmap (findButtonMaybe buttons) inputValues

findButtonMaybe :: [Button] -> Value -> Maybe (Digit, Presses)
findButtonMaybe buttons target = do
    button      <- find (\button -> elem target (values button)) buttons
    targetIndex <- elemIndex target $ values button
    Just (name button, targetIndex + 1)

reverseTapsMaybe :: DaPhone -> Char -> Maybe [(Digit, Presses)]
reverseTapsMaybe (DaPhone buttons) input = do
    inputValue         <- toMaybeValues input
    foundButtonPresses <- sequence $ findButtonsMaybe buttons inputValue
    Just foundButtonPresses

cellPhonesDeadMaybe :: DaPhone -> String -> Maybe [(Digit, Presses)]
cellPhonesDeadMaybe phone input = sequence $ do
    inputChar <- input
    sequence $ reverseTapsMaybe phone inputChar

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\digitPresses acc -> (snd digitPresses) + acc) 0

mostPopularLetter :: String -> Char
mostPopularLetter input = fst $ maximumBy compareGroups (toLetterCounts input)

compareGroups :: (a, Int) -> (a, Int) -> Ordering
compareGroups (_, aCount) (_, bCount) | aCount < bCount = LT
                                      | aCount > bCount = GT
                                      | otherwise       = EQ

toLetterCounts :: String -> [(Char, Int)]
toLetterCounts input = do
    group <- groupBy (==) input
    [(head group, length group)]

count :: Eq a => [a] -> [(a, Int)]
count input = do
    group <- groupBy (==) input
    [(head group, length group)]

coolestLtr :: [String] -> Char
coolestLtr inputStrings =
    fst $ maximumBy compareGroups $ count $ sort $ intercalate "" inputStrings

coolestWord :: [String] -> String
coolestWord inputStrings =
    fst
        $ maximumBy compareGroups
        $ count
        $ sort
        $ words
        $ filter (\c -> isLetter c || isSpace c)
        $ intercalate " " inputStrings

