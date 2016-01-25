module GlobRegEx (
	globToRegex,
	matchesGlob
)where

import Text.Regex.Posix ((=~))
import Data.Char

matchesGlob :: FilePath -> String -> Bool -> Bool
matchesGlob name pat sensitive = name =~ globToRegex pat sensitive

globToRegex :: String -> Bool -> String
globToRegex cs sensitive = '^' : globToRegex' cs  sensitive ++ "$"

globToRegex' :: String -> Bool -> String
globToRegex' "" _ = ""
globToRegex' ('*':cs) sensitive = ".*" ++ globToRegex' cs sensitive
globToRegex' ('?': cs)  sensitive = '.' : globToRegex' cs sensitive
globToRegex' ('[':'!':c:cs) sensitive = "[^" ++ c : charClass cs sensitive
globToRegex' ('[':c:cs) sensitive = '[' : c : charClass cs sensitive
globToRegex' ('[':_) _ = error "Unterminated character class"
globToRegex' (c:cs) sensitive = escape c sensitive ++ globToRegex' cs sensitive

escape :: Char -> Bool -> String
escape c sensitive	| c `elem` regexChars = '\\' : [c]
				     				| otherwise = findCase c sensitive
		where
			regexChars = "\\+()^$.{}]|"
			findCase :: Char -> Bool -> String
			findCase c sensitive | sensitive = [c]
													 | isUpper c =  "(" ++ [c] ++ "|" ++ [(toLower c)] ++ ")"
													 | otherwise =  "(" ++ [c] ++ "|" ++ [(toUpper c)] ++ ")"

charClass :: String -> Bool -> String
charClass (']':cs) sensitive = ']' : globToRegex' cs sensitive
charClass (c:cs) sensitive = c : charClass cs sensitive
charClass [] _ = error "Unterminated character class"
