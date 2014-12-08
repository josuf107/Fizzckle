import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Maybe
import Fizz.Core
import Fizz.Store

type Text = Text.Text
type ByteString = BS.ByteString
type Vector = Vector.Vector

data BankEntryType = Deposit | Withdrawal deriving (Show, Eq)
data BankEntry
    = BankEntry
    { bankEntryDate :: String
    , bankEntryType :: BankEntryType
    , bankEntryValue :: Double
    , bankEntryInfo :: String
    } deriving (Show, Eq)

findMissing :: Journal -> [BankEntry] -> [BankEntry]
findMissing j be = undefined

parseFile :: FilePath -> IO (Either String [BankEntry])
parseFile = fmap parseLines . BS.readFile

parseLines :: ByteString -> Either String [BankEntry]
parseLines
    = fmap (catMaybes . fmap toBankEntry . Vector.toList)
    . Csv.decode Csv.HasHeader

toBankEntry :: (Text, Text, Text, Maybe Double, Maybe Double, Text) -> Maybe BankEntry
toBankEntry tuple =
    case tuple of
        (date, _, _, Just withdraw, _, info) -> Just
            $ makeEntry date Withdrawal withdraw info
        (date, _, _, _, Just deposit, info) -> Just
            $ makeEntry date Deposit deposit info
        _ -> Nothing
    where
        makeEntry date t value info = BankEntry (Text.unpack date) t (abs value) (Text.unpack info)
