module Throwaway where
import Data.Char (toUpper)

-- Basic add    
add :: Int -> Int -> Int
add x y = x + y


-- Check for odd and even number
checkNumber :: Int -> String
checkNumber y =
    if (mod y 2) == 0
        then "even"
        else "odd"


-- Add numbers within a list
sumOfList :: Int -> [Int] -> Int
sumOfList total lst =
    if (lst == [])
        then total
        else sumOfList (total + (head lst)) (tail lst)


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