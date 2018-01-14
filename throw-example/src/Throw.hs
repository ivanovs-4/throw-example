-- | Client for NEM NIS API
module Throw where

import Control.Exception
-- import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Function
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Text as T

data Result = Result { unResult :: Int }
    deriving (Show)

analyseResult :: Result -> IO Result
analyseResult r@Result {..} = do
    case unResult of
        1 -> do
            -- putStrLn [qc|Answer: {r}|]
            pure r
        _ -> do
            -- putStrLn [qc|Answer: {r}|]
            -- fail [qc|Answer: {r}|]

            throwIO . CustomError $ T.pack . show $ r
            -- !!! ^ Почему здесь зависает, а на fail работает?

newtype CustomError = CustomError Text
instance Show CustomError
instance Exception CustomError

run :: IO ()
run = do
    r <- try $ analyseResult $ Result 2
    case r of
        Left (e :: SomeException) -> print r
        Right _ -> print r
    putStrLn "Done"
