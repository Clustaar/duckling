{-# LANGUAGE OverloadedStrings #-}

import Prelude -- imported by default btw
import Duckling.Core
import Control.Monad.IO.Class (liftIO)
import System.Posix.Env.ByteString (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Aeson (decode, encode)
import Data.ByteString.Internal (ByteString)
-- import QUALIFIED force to use "as_value.function"
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Unicode
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
    -- get arguments in [ByteString]
    args <- getArgs
    -- extract the 3 arguments and use parseHandler:
    -- sentence = string that represent sentence to parse
    -- language = string that represent parser's language (FR, EN, ...)
    -- dimensions = string that represent list of dimensions to use
    case args of
        [sentence, language, dimensions] -> parseHandler sentence language dimensions 
        _ -> putStrLn "3 args"

-- function that take sentence, language, dimensions to perform parsing and print result
parseHandler :: ByteString -> ByteString -> ByteString  -> IO ()
parseHandler sentence language dimensions = do
    -- liftIO allows to use tzs (get result of IO operation)
    -- currentReftime -> DucklingCore
    refTime <- liftIO $ currentReftime (HashMap.fromList []) " " 
    let
        context = Context
            { referenceTime = refTime
            , locale = makeLocale (parseLang language) Nothing
            }
        options = Options {withLatent = False}
        
        parsedResult = parse (Unicode.decodeUtf8 sentence) context options (parseDims dimensions)
    
    LBSChar.putStrLn (encode parsedResult)
    where
        parseLang :: ByteString -> Lang
        parseLang l = fromMaybe FR $ readMaybe $ Text.unpack $ Text.toUpper $ Unicode.decodeUtf8 l

        -- map on list to return list of Some Dimensions from list of labels
        -- (fromName return Maybe Some Dimension => mapMaybe instead of map)
        parseDims :: ByteString -> [Some Dimension]
        parseDims d = mapMaybe fromName (decodeDims d)
            where
                -- transform dimensions (str) to LazyBytestring, decode (-> [Maybe String] ?)
                -- and create List from input if it's possible else return empty list
                decodeDims :: ByteString -> [Text.Text]
                decodeDims ds = fromMaybe [] $ decode $ LBS.fromStrict $ ds
