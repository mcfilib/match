{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Lib (
  -- * Sanitisation
   sanitise
  -- * Compilation
  , compile
  , compile'
  -- * Parsing
  , decode
  -- * Matching
  , match
  , match'
  -- * Pattern-level optimisation
  , optimise
  -- * DSL
  , Capture(..)
  , Expr(..)
) where

import           Control.Applicative ((<|>), many)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, char, decimal, parseOnly, peekChar, takeWhile1)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import           Data.String.Conv (toS)
import           Data.Text (Text)
import           Text.Regex.PCRE.Heavy (gsub, re)
import qualified Text.Regex.PCRE.Light as PCRE

--------------------------------------------------------------------------------

-- | The non-negative integer denotes the index into the token list associated
-- with the rule to which the pattern belongs.
type Index = Int

-- | The precise number of whitespace characters that must be appear between
-- text literals in order to match the associated token capture sequence.
type WhitespaceCount = Int

-- | A token capture sequence identifies the text extracted from the input.
data Capture = Simple          {-# UNPACK #-} !Index
               -- ^ Token capture and index.
             | SpaceLimitation {-# UNPACK #-} !Index !WhitespaceCount
               -- ^ Space limitation token capture with index and number of
               -- whitespace characters between adjacent text literals.
             | Greedy          {-# UNPACK #-} !Index
               -- ^ Greedy token capture with index.
             | TerminalSpaceLimitation
               -- ^ Special case of the space limitation modifier that may
               -- occur at the end of a pattern and indicates no more input.
             deriving (Show, Eq)

-- | An expr is a semantic unit in our pattern language.
data Expr = ExprText !Text
            -- ^ Text literal.
          | ExprCapture !Capture
            -- ^ Token cature.
          deriving (Show, Eq)

-- | A pattern is a linear structure containing a list of expressions.
type Pattern = [Expr]

--------------------------------------------------------------------------------

-- | Compile our pattern down to a pcre-light regex.
compile :: Pattern -> PCRE.Regex
compile pattern = PCRE.compile (compile' pattern) []

--------------------------------------------------------------------------------

-- | Compile our pattern down to a regex.
--
-- NOTE: intended for testing purposes.
compile' :: Pattern -> BS.ByteString
compile' pattern = BS.intercalate "" (fromExpr <$> pattern)
  where
    fromExpr :: Expr -> BS.ByteString
    fromExpr (ExprText x) =
      toS x
    fromExpr (ExprCapture (Simple _)) =
      "(?:.+)"
    fromExpr (ExprCapture (SpaceLimitation _ 0)) =
      "(?:\\S+\\s{0})"
    fromExpr (ExprCapture (SpaceLimitation _ n)) =
      let x = replicate (n + 1) "\\S+" in
      "(?:" <> BS.intercalate "\\s" x <> ")"
    fromExpr (ExprCapture TerminalSpaceLimitation) =
      "(?:\\S+\\s{0}$)"
    fromExpr (ExprCapture (Greedy _)) =
      "(?:.*)"

--------------------------------------------------------------------------------

-- | Parses our pattern language.
decode :: Text -> Either String Pattern
decode = parseOnly parser
  where
    parser :: Parser Pattern
    parser = many (capture <|> text)

    text :: Parser Expr
    text = do
      xs <- takeWhile1 (/= '%')
      next <- peekChar
      case next of
        -- If at end of input do not go any further.
        Nothing -> return $ ExprText xs
        -- Otherwise check if we have a capture and continue.
        Just _  -> lookAhead capture *> pure (ExprText xs) <|>
          char '%' *> pure (ExprText (xs <> "%"))

    capture :: Parser Expr
    capture = simpleCapture <|> spaceLimitationCapture <|> greedyCapture

    simpleCapture :: Parser Expr
    simpleCapture = do
      _ <- char '%'
      _ <- char '{'
      i <- decimal
      _ <- char '}'
      return $ ExprCapture (Simple i)

    spaceLimitationCapture :: Parser Expr
    spaceLimitationCapture = do
      _ <- char '%'
      _ <- char '{'
      i <- decimal
      _ <- char 'S'
      s <- decimal
      _ <- char '}'
      return $ ExprCapture (SpaceLimitation i s)

    greedyCapture :: Parser Expr
    greedyCapture = do
      _ <- char '%'
      _ <- char '{'
      i <- decimal
      _ <- char 'G'
      _ <- char '}'
      return $ ExprCapture (Greedy i)

--------------------------------------------------------------------------------

-- | Match our pcre-light regex against a bytestring.
match :: PCRE.Regex -> BS.ByteString -> Maybe [BS.ByteString]
match regex input = PCRE.match regex input []

--------------------------------------------------------------------------------

-- | Matches our pattern against a bytestring.
--
-- NOTE: intended for testing purposes.
match' :: Pattern -> BS.ByteString -> Maybe [BS.ByteString]
match' pattern = match (compile pattern)

--------------------------------------------------------------------------------

-- | Optimises patterns at the token level.
--
-- NOTE: this reverses the list at worst twice - if it turns out that patterns
--       can be very long, you might consider using another data structure such
--       as a Vector.
optimise :: Pattern -> Pattern
optimise pattern = case reverse pattern of
  -- If we have a zero-whitespace modifier at the end of our pattern, treat it
  -- as Terminal.
  (ExprCapture (SpaceLimitation _ 0) : xs) -> reverse $ ExprCapture TerminalSpaceLimitation : xs
  -- Otherwise, perform no optimisation.
  _                                        -> pattern

--------------------------------------------------------------------------------

-- | Handle undesirable input.
sanitise :: Pattern -> Pattern
sanitise = fmap escape
  where
    escape :: Expr -> Expr
    escape capture@(ExprCapture _) = capture
    escape (ExprText text) = ExprText $ gsub regex (\x -> ("\\" <> x) :: Text) text

    regex :: PCRE.Regex
    regex = [re|[|\\{}()[\]^$+*?.]|]
