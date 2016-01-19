module Assignments (
    Configuration(Configuration),
    Assignment(Assignment),
    UserIdentifier,
    Year,
    Submission(Submission),
    Type(Homework , Exam , Project),
    getConfiguration,
    listSubmissions,
    getSubmission,
    listFiles,
    createAssignment,
    getSubmissionPath,
    upload) where
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import System.Directory
import Data.List
import qualified Data.Text as T

root = "V:/Univeras/7pusmetis/Haskell/project/PUH_project/"
confName = "conf"

-- | A user identifier (not DB id) like a username or JMBAG 
type UserIdentifier = String

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

-- | An assignment type
data Type = Homework | Exam | Project deriving Eq

instance Show Type where
   show Homework = "homework" 
   show Exam = "exam"
   show Project = "project"

-- | A an assignment configuration data structure
-- | Uses Data.Time.UTCTime
-- | If files is an empty list, ANY number of files are OK
data Configuration = Configuration {
    published    :: UTCTime, -- When to publish
    deadline     :: UTCTime, -- Submission deadline
    lateDeadline :: UTCTime, -- Late submission deadline
    files        :: [String], -- File names to expect
    minScore     :: Double, -- Minimum achievable
    maxScore     :: Double, -- Maximum achievable
    required     :: Double -- Score req to pass
}

instance Show Configuration where
    show c = "published = " ++ show ( published c )++ "\n" ++
             "deadline = " ++ show ( deadline c ) ++ "\n" ++
             "lateDeadline = " ++ show ( lateDeadline c )++ "\n" ++
             "files = " ++ show ( files c) ++ "\n" ++
             "minScore = " ++ show ( minScore c) ++ "\n" ++
             "maxScore = " ++ show ( maxScore c) ++ "\n" ++
             "required = " ++ show ( required c) 

-- | An assignment descriptor
data Assignment = Assignment {
    year   :: Year,
    aType  :: Type,
    number :: Int
} deriving (Show, Eq)

data Submission = Submission {
    assignment     :: Assignment,
    userId         :: UserIdentifier,
    submittedFiles :: [FilePath]
} deriving Show

confFromString str = Configuration {
      published = getUtc $ lv !! 0,
      deadline = getUtc $ lv !! 1,
      lateDeadline = getUtc $ lv !! 2,
      files = read ((lv !! 3) !! 2) :: [String],
      minScore = read ((lv !! 4) !! 2) :: Double,
      maxScore = read ((lv !! 5) !! 2) :: Double,
      required = read ((lv !! 6) !! 2) :: Double
   }
   where lv = map words $ lines str
         getUtc s = read (intercalate " " $ drop 2 s) :: UTCTime

getPathFromAssignment :: Assignment -> FilePath
getPathFromAssignment a = root ++ (show $ year a) ++ "/" ++ (show $ aType a) ++ "/" ++ (show $ number a)

getConfiguration :: Assignment -> IO Configuration
getConfiguration asgn = do 
    fileExist  <- doesFileExist $ (getPathFromAssignment asgn) ++ "/" ++ confName
    if not fileExist
    then error "Configuration file does not exist!"
    else do
        cnf <- readFile ((getPathFromAssignment asgn) ++ "/" ++ confName) 
        return $ confFromString cnf

listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions asgn = do
    let path = (getPathFromAssignment asgn)
    check <- doesDirectoryExist path 
    if check
    then do
      contents <- getDirectoryContents path
      let submissions = filter (\x -> x /= confName && x /= "Assignment.pdf" && x /= "." && x /= "..") contents
      return submissions
    else
      error "The assignment does not exist."

getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission asgn user = do
    let path = (getPathFromAssignment asgn) ++ "/" ++ user ++ "/"
    check <- doesDirectoryExist path 
    if check
    then do
      contents <- getDirectoryContents path
      let sFiles = filter (\x -> isInfixOf ".hs" x) contents
      let sbm = Submission { assignment = asgn, userId = user, submittedFiles = sFiles }
      return sbm
    else
      error "The user has not submitted this assignment"

-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment asg conf pdf = do 
    let path = getPathFromAssignment asg 
    createDirectoryIfMissing True path 
    copyFile pdf $ path ++ "/Assignment.pdf"
    writeFile (path ++ "/" ++ confName) $ show conf
    
-- Uses Text from Data.Text
-- T.pack   :: String -> Text
-- T.unpack :: Text -> String
upload :: Submission -> T.Text -> String -> IO Submission
upload sub body name = do
    let path = (getPathFromAssignment $ assignment sub) ++ "/" ++ (userId sub)
    createDirectoryIfMissing True path 
    writeFile (path ++ "/" ++ name) (T.unpack body)
    return $ sub {submittedFiles = nub(name:(submittedFiles sub))}
    
listFiles :: Submission -> IO [FilePath]
listFiles s = do
  return $ submittedFiles s
    
getSubmissionPath :: Submission -> FilePath
getSubmissionPath s = getPathFromAssignment (assignment s) ++ "/" ++ (userId s)    
    
-- Test

cnfg = Configuration {
  published = read "2011-11-19 18:28:52.607875 UTC" :: UTCTime,
  deadline = read "2011-11-19 18:28:52.607875 UTC" :: UTCTime,
  lateDeadline = read "2011-11-19 18:28:52.607875 UTC" :: UTCTime,
  files = ["exercises.hs", "homework.hs"],
  minScore = 0.0,
  maxScore = 8.0,
  required = 4.0
}

asgn = Assignment {
  year = 2015,
  aType = Homework,
  number = 1
}

subm = Submission {
  assignment = asgn,
  userId = "Taurius",
  submittedFiles = []
}


main :: IO ()
main = putStrLn "Hello Haskell"





























    
    
    
    
    
    
    
    
    
    
    
    
    
    
  