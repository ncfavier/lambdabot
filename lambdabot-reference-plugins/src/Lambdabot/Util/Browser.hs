{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- | URL Utility Functions

module Lambdabot.Util.Browser
    ( urlPageTitle
    , browseLB
    ) where

import Codec.Binary.UTF8.String
import Control.Monad.Trans
import Data.ByteString.Encoding qualified as BE
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.UTF8 qualified as B
import Data.CaseInsensitive
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import Lambdabot.Config
import Lambdabot.Config.Reference
import Lambdabot.Monad
import Lambdabot.Util (limitStr)
import Network.Browser hiding (request)
import Network.HTTP.Media
import Network.HTTP.Simple
import Text.Html.Encoding.Detection
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

-- | Run a browser action with some standardized settings
browseLB :: MonadLB m => BrowserAction conn a -> m a
browseLB act = lb $ do
    proxy' <- getConfig proxy
    liftIO . browse $ do
        setOutHandler (const (return ()))
        setErrHandler (const (return ()))

        setAllowRedirects True
        setMaxRedirects (Just 5)
        setUserAgent "LambdaBot"
        setProxy proxy'
        act

-- | Limit the maximum title length to prevent jokers from spamming
-- the channel with specially crafted HTML pages.
maxTitleLength :: Int
maxTitleLength = 350

-- | Fetches a page title suitable for display.  Ideally, other
-- plugins should make use of this function if the result is to be
-- displayed in an IRC channel because it ensures that a consistent
-- look is used (and also lets the URL plugin effectively ignore
-- contextual URLs that might be generated by another instance of
-- lambdabot; the URL plugin matches on 'urlTitlePrompt').
urlPageTitle :: MonadLB m => String -> m (Maybe String)
urlPageTitle = fmap (fmap (limitStr maxTitleLength)) . rawPageTitle

-- | Fetches a page title for the specified URL.  This function should
-- only be used by other plugins if and only if the result is not to
-- be displayed in an IRC channel.  Instead, use 'urlPageTitle'.
rawPageTitle :: MonadLB m => String -> m (Maybe String)
rawPageTitle url = do
    request <- liftIO $ addRequestHeader "Accept-Language" "en,*"
                      . addRequestHeader "User-Agent" "LambdaBot"
                      <$> parseRequest (takeWhile (/='#') url)
    response <- httpBS request
    case getResponseStatusCode response of
        200 | ct:_ <- mapMaybe (parseAccept @MediaType) $ getResponseHeader "Content-Type" response
            , ct `matches` "text/html" -> liftIO $ do
                let body = getResponseBody response
                encoding <- maybe (return BE.utf8) BE.mkTextEncoding $ asum
                    [ B.toString . original <$> ct /. "charset"
                    , detectBom (BL.fromStrict body)
                    , detectMetaCharset . BL.take 1024 . BL.fromStrict $ body ]
                return $ extractTitle $ BE.decode encoding body
        _ -> return Nothing


-- | Given a text/html server response, return the text in
-- between the <title> tag.
extractTitle :: T.Text -> Maybe String
extractTitle = content . tags where
    tags = closing . opening . canonicalizeTags . parseTags
    opening = dropWhile (not . tagOpenLit "title" (const True))
    closing = takeWhile (not . tagCloseLit "title")

    content = maybeText . format . innerText
    format = T.unwords . T.words
    maybeText t | T.null t = Nothing
                | otherwise = Just (encodeString (T.unpack t))
