module Comp.Printf
import Data.String
%default total

data Format : Type where
  FInt : Format
  FString : Format
  FChar : Format
  FLit : String -> Format
  FOther : Char -> Format

parseFormat : List Char -> List Format
parseFormat [] = []
parseFormat ('%' :: 'd' :: rest) = FInt :: parseFormat rest
parseFormat ('%' :: 's' :: rest) = FString :: parseFormat rest
parseFormat ('%' :: 'c' :: rest) = FChar :: parseFormat rest
parseFormat ('%' :: c :: rest) = FOther c :: parseFormat rest
parseFormat (c :: rest) = FLit (singleton c) :: parseFormat rest

parseSig : List Format -> Type
parseSig [] = String
parseSig (FInt :: xs) = Int -> parseSig xs
parseSig (FString :: xs) = String -> parseSig xs
parseSig (FChar :: xs) = Char -> parseSig xs
parseSig (FLit _ :: xs) = parseSig xs
parseSig (FOther _ :: xs) = parseSig xs

parseType : String -> Type
parseType str = parseSig (parseFormat (unpack str))

format : (fmt : List Format) -> String -> parseSig fmt
format [] acc = acc
format (FInt :: fs) acc = \i => format fs (acc ++ show i)
format (FString :: fs) acc = \s => format fs (acc ++ s)
format (FChar :: fs) acc = \c => format fs (acc ++ singleton c)
format (FLit s :: fs) acc = format fs (acc ++ s)
format (FOther c :: fs) acc = format fs (acc ++ singleton c)

printf : (fmt : String) -> parseType fmt
printf fmt = format (parseFormat (unpack fmt)) ""

main : IO ()
main = do
  pure ()
