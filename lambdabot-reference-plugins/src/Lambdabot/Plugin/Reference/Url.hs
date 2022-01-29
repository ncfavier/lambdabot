{-# LANGUAGE PatternGuards #-}
-- | Fetch URL page titles of HTML links.
module Lambdabot.Plugin.Reference.Url (urlPlugin) where

import Lambdabot.Plugin
import Lambdabot.Util.Browser

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Text.Regex.TDFA

urlPlugin :: Module Bool
urlPlugin = newModule
    { moduleCmds = return
        [ (command "url-title")
            { help = say "url-title <url>. Fetch the page title."
            , process =
                  maybe (say "Url not valid.") (mbSay <=< fetchTitle)
                . containsUrl
            }
        , (command "url-on")
            { privileged = True
            , help = say "url-on: enable automatic URL summaries"
            , process = const $ do
                writeMS True
                say "Url enabled"
            }
        , (command "url-off")
            { privileged = True
            , help = say "url-off: disable automatic URL summaries"
            , process = const $ do
                writeMS False
                say "Url disabled"
            }
        ]
    , moduleDefState              = return True -- url on
    , moduleSerialize             = Just stdSerial

    , contextual = \text -> do
      alive <- lift readMS
      if alive && (not $ areSubstringsOf ignoredStrings text)
        then case containsUrl text of
               Nothing  -> return ()
               Just url -> mbSay =<< fetchTitle url
        else return ()
    }

mbSay :: Maybe String -> Cmd (ModuleT Bool LB) ()
mbSay = maybe (return ()) say

------------------------------------------------------------------------

-- | The string that I prepend to the quoted page title.
urlTitlePrompt :: String
urlTitlePrompt = "Title: "

-- | Fetch the title of the specified URL.
fetchTitle :: MonadLB m => String -> m (Maybe String)
fetchTitle url = fmap (fmap (urlTitlePrompt ++)) (urlPageTitle url)

-- | List of strings that, if present in a contextual message, will
-- prevent the looking up of titles.  This list can be used to stop
-- responses to lisppaste for example.  Another important use is to
-- another lambdabot looking up a url title that contains another
-- url in it (infinite loop).  Ideally, this list could be added to
-- by an admin via a privileged command (TODO).
ignoredStrings :: [String]
ignoredStrings =
    [urlTitlePrompt]         -- Ignore others like me

-- | Suffixes that should be stripped off when identifying URLs in
-- contextual messages.  These strings may be punctuation in the
-- current sentence vs part of a URL.  Included here is the NUL
-- character as well.
ignoredUrlSuffixes :: [String]
ignoredUrlSuffixes = [".", ",", ";", ")", "\"", "\1", "\n"]

-- | Searches a string for an embedded URL and returns it.
containsUrl :: String -> Maybe String
containsUrl text = do
    mr <- matchM begreg text
    let kind = mrMatch mr
        rest = mrAfter mr
        url = takeWhile (`notElem` " \n\t\v") rest
    return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
    where
        begreg = makeRegexOpts opts defaultExecOpt "https?://"
        opts = defaultCompOpt { caseSensitive = False }

-- | Utility function to remove potential suffixes from a string.
-- Note, once a suffix is found, it is stripped and returned, no other
-- suffixes are searched for at that point.
stripSuffixes :: [String] -> String -> String
stripSuffixes []   str   = str
stripSuffixes (s:ss) str
    | isSuffixOf s str   = take (length str - length s) $ str
    | otherwise          = stripSuffixes ss str


-- | Utility function to check of any of the Strings in the specified
-- list are substrings of the String.
areSubstringsOf :: [String] -> String -> Bool
areSubstringsOf = flip (any . flip isSubstringOf)
    where
      isSubstringOf s str = any (isPrefixOf s) (tails str)
