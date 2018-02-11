module ArangoDB.Utils.Naming where

import Data.Char (toLower, isUpper, isDigit)
import Data.List (intercalate)

-- | Default name modifier.
-- Transforms to @snake_case@ and cuts longest common whole-word prefix.
--
-- >>> nameModifier "Dog" "dogName"
-- "name"
-- >>> nameModifier "PersonEntry" "personLastName"
-- "last_name"
-- >>> nameModifier "MessageStatus" "MessageSent"
-- "sent"
-- >>> nameModifier "SMSEntry" "smsExpiresAt"
-- "expires_at"
-- >>> nameModifier "Progress" "progress75"
-- "75"
nameModifier :: String -> String -> String
nameModifier prefix name = brokenWord ++ suffix
  where
    (common, _, suffix) = commonPrefix (toSnakeCase prefix ++ "_") (toSnakeCase name)
    brokenWord = reverse (takeWhile (/= '_') (reverse common))

-- | Strip longest common prefix of to lists.
--
-- >>> commonPrefix "MessageStatus" "MessageDelivered" :: (String, String, String)
-- ("Message","Status","Delivered")
-- >>> commonPrefix "MessageStatus" "MessageSent" :: (String, String, String)
-- ("MessageS","tatus","ent")
--
-- prop> commonPrefix xs xs == (xs, [], [])
-- prop> commonPrefix (xs ++ ys) (xs ++ zs) == (\(as, bs, cs) -> (xs ++ as, bs, cs)) (commonPrefix ys zs)
-- prop> commonPrefix [] xs == ([], [], xs)
-- prop> commonPrefix xs [] == ([], xs, [])
commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
commonPrefix (x:xs) (y:ys)
  | x == y    = (x : prefix, xs', ys')
  | otherwise = ([], x:xs, y:ys)
  where
    (prefix, xs', ys') = commonPrefix xs ys
commonPrefix xs ys = ([], xs, ys)

-- | Convert @CamelCase@ to @snake_case@.
--
-- >>> toSnakeCase "Call"
-- "call"
-- >>> toSnakeCase "CampaignId"
-- "campaign_id"
-- >>> toSnakeCase "SMSId"
-- "sms_id"
-- >>> toSnakeCase "progress75"
-- "progress_75"
toSnakeCase :: String -> String
toSnakeCase = map toLower . intercalate "_" . splitCamelWords
  where
    splitCamelWords = reverse . splitWordsReversed . reverse

    splitWordsReversed :: String -> [String]
    splitWordsReversed [] = []
    splitWordsReversed rs
      | null ls   = reverse us : splitWordsReversed urs
      | otherwise = case lrs of
                      []     -> [reverse ls]
                      (c:cs) -> (c : reverse ls) : splitWordsReversed cs
      where
        (ls, lrs) = span (not . isBorder) rs
        (us, urs) = span isBorder rs
        isBorder c = isUpper c || isDigit c
