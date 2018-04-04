module Throwaway where
import Data.Char (toUpper)

-- Basic add    
add :: Int -> Int -> Int
add x y = x + y


-- Check for odd and even number
checkNumber :: Int -> String
checkNumber y
    | mod y 2 == 0 = "even"
    | otherwise = "odd"


-- Add numbers within a list
sumOfList :: Int -> [Int] -> Int
sumOfList total lst
    | lst == [] = total
    | otherwise sumOfList (total + (head lst)) (tail lst)


-- Add number within a list, better version
sumOfListInternal :: Int -> [Int] -> Int
sumOfListInternal total lst =
    if (lst == [])
        then total
        else sumOfListInternal (total + (head lst)) (tail lst)
sumOfListBetter :: [Int] -> Int
sumOfListBetter lst = sumOfListInternal 0 lst


-- Add just even numbers within a list
sumOfEven :: Int -> [Int] -> Int
sumOfEven total lst =
    if (lst == [])
        then total
        else if (mod (head lst) 2) == 0
            then sumOfEven (total + (head lst)) (tail lst)
            else sumOfEven total (tail lst)

-- Double numbers in a list
doubleList :: [Int] -> [Int] -> [Int]
doubleList processedList remainingList =
    if (remainingList == [])
        then processedList
        else doubleList (processedList ++ [(head remainingList) * 2]) (tail remainingList)


-- Uppercase some strings

toUppercase :: String -> String -> String
toUppercase processedString remainingString =
    if (remainingString == [])
        then processedString
        else toUppercase (processedString ++ [toUpper (head remainingString)]) (tail remainingString)


-- Checking to see if characters in string are in another string

isCharPresent :: Char -> [Char] -> Bool
isCharPresent needle remainingString
  | (remainingString == []) = False
  | (needle == (head remainingString)) = True
  | otherwise = isCharPresent needle (tail remainingString)

allCharsPresent :: [Char] -> [Char] -> Bool
allCharsPresent remainingNeedles haystack
  | (remainingNeedles == []) = True
  | isCharPresent (head remainingNeedles) haystack = allCharsPresent (tail remainingNeedles) haystack
  | otherwise = False


-- create a list of even numbers to N
evenList :: Int -> [Int] -> [Int]
evenList n lst
  | n==0 = lst
  | mod n 2 == 0 = evenList (n - 1) (n:lst)
  | otherwise = evenList (n - 1) lst


-- check for leap year
leapYearCheck :: Int -> Bool
leapYearCheck year
    | mod year 400 == 0 = True
    | mod year 100 == 0 = False
    | mod year 4 == 0 = True
    | otherwise = False