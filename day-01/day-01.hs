import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

findTargetWithDepth :: Int -> Int -> Set Int -> Maybe Int
findTargetWithDepth target 1 numbers = if S.member target numbers then Just target else Nothing
findTargetWithDepth target depth numbers = asum . fmap findRemaining . toList $ numbers
  where
    findRemaining this = (*this) <$> findTargetWithDepth (target - this) (depth - 1) (S.delete this numbers)

part1 :: Set Int -> Int
part1 = fromJust . findTargetWithDepth 2020 2

part2 :: Set Int -> Int
part2 = fromJust . findTargetWithDepth 2020 3

main :: IO ()
main = do
  numbers <- S.fromList . map read . lines <$> readFile "input.txt" :: IO (Set Int)

  putStrLn . show $ part1 numbers
  putStrLn . show $ part2 numbers

