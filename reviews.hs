import System.Random
import Assignments
import Data.List
import qualified Data.Text as T

-- | A user’s role or authorization level as a reviewer
data Role = Student | Staff deriving (Eq, Ord, Show)

-- | A review assignment representation
data ReviewAssignment = ReviewAssignment{ 
    reviewer :: UserIdentifier
    , reviewee :: UserIdentifier
    , role :: Role
    , assignment :: Assignment
} deriving (Show)

-- | A finished review
data Review = Review{ 
    reviewAssignment :: ReviewAssignment
    , score :: Double
    , text :: T.Text
} deriving Show

asgn = Assignment 2016 Homework 1

assignmentsFile = "Assignments.txt"

-- | Takes an Assignment, a list of reviewer identifiers and a
-- | list of reviewee identifiers and assigns N reviewees for each
-- | reviewer. It makes sure that a user never reviews themselves.
-- | The reviewer is assigned the reviews with the provided role.
assignNReviews :: Assignment -> [UserIdentifier] -> [UserIdentifier] -> Int -> Role -> IO [ReviewAssignment]
assignNReviews asg reviewers reviewees n role = do
    let reviewees' = take (n * (length reviewers)) (concat $ repeat reviewees)
    let pairs = getP reviewers reviewees' n
    let rez = map (\(x,y) -> (ReviewAssignment x y role asg)) pairs
    return rez        

shuffle :: Eq a => [a] -> [a] -> IO ([a])
shuffle [] rez = do
    return rez
shuffle xs rez = do
    el <- randomRIO (0, (length xs)-1)
    let el' = xs !! el
    let xs' = delete el' xs
    shuffle xs' (el':rez) 
    
getP :: Eq a => [a] -> [a] -> Int -> [(a,a)]
getP x0 y0 n0 = getPairs x0 y0 n0 
    where getPairs [] _ _ = []
          getPairs (x:xs) ys 0 = getPairs xs ys n0
          getPairs xs@(x:_) (y:ys) n
              | x /= y = (x,y):(getPairs xs ys (n-1))
              | otherwise = getPairs xs (ys ++ [y]) n
              
aaa = ["Tadas", "Andrius", "Taurius"]

assignReviews :: Assignment -> [UserIdentifier] -> [UserIdentifier] -> Role -> IO [ReviewAssignment]
assignReviews asg reviewers reviewees role = do
    reviewees' <- shuffle reviewees []
    let n = div (length reviewees) (length reviewers)
    let reviewers' = concat $ replicate (n+1) reviewers
    let rez = map (\(x,y) -> (ReviewAssignment x y role asg)) $ zip reviewers' reviewees'
    return rez 


-- | Stores a list of review assignments into a database or
-- | file system.
storeAssigments :: [ReviewAssignment] -> IO ()
storeAssigments xs = do
    let xs' = concatMap ((++"\n") . show) xs 
    writeFile assignmentsFile  xs'
    return ()  

-- | Retrieves all ReviewAssignments for an Assignment from
-- | a database or file system.
assignedReviews :: Assignment -> IO [ReviewAssignment]
assignedReviews asg = do
    content <- readFile assignmentsFile
    let assignments = filter ((==asg) . assignment) $map (getReviewFromStr) $ lines content
    return assignments

-- Parse ReviewAssignment from string    
getReviewFromStr str = ReviewAssignment (read $ w !! 3) (read $ w !! 6) (getRole $ w !! 9) 
                            (Assignment (read (w !! 15) :: Integer) 
                                (getaType (w !! 18)) (read (w !! 21) :: Int)) 
    where w = words $ filter (\x -> x/=',' && x /= '}') str

-- Returns Role from given String
getRole :: String -> Role
getRole "Student" = Student
getRole "Staff"   = Staff
getRole _         = error "Can't parse role"

-- Returns assignment type from given String
getaType :: String -> Type
getaType "homework" = Homework
getaType "exam"     = Exam
getaType "project"  = Project
getaType _          = error "Can't parse assignment type"

-- | Retrieves all ReviewAssignments for an Assignment and
-- | a UserIdentifier, i.e. all the reviews for that assigment
-- | the user has to perform.
assignmentsBy :: Assignment -> UserIdentifier -> IO [ReviewAssignment]
assignmentsBy asg userId = do
    filteredAsgn <- assignedReviews asg
    let rez = filter ((==userId) . reviewer) filteredAsgn
    return rez

-- | Retrieves all ReviewAssignments for an Assignment and
-- | a UserIdentifier, i.e. all the reviews for that assignment
-- | where the user is a reviewee.
assignmentsFor :: Assignment -> UserIdentifier -> IO [ReviewAssignment]
assignmentsFor asg userId = do
    filteredAsgn <- assignedReviews asg
    let rez = filter ((==userId) . reviewee) filteredAsgn
    return rez





