import Assignments
import Reviews
import Email
--import User


import Data.List
import qualified Data.Text as T
import Data.Time.Clock

asg1 = Assignment 2015 Homework 1
asg2 = Assignment 2015 Homework 2

sub1 = Submission asg1 "12345" []
sub2 = Submission asg2 "78910" []

asgn = Assignment 2016 Homework 1

rev = ReviewAssignment "Luka124" "3984344" Student asgn

re = Review rev 9.5 (T.pack "Good job")

cnfg = Configuration (read "2011-11-19 18:28:52.607875 UTC" :: UTCTime)
                     (read "2011-11-19 18:28:52.607875 UTC" :: UTCTime)
                     (read "2011-11-19 18:28:52.607875 UTC" :: UTCTime)
                     ["exercises.hs", "homework.hs"]
                     0.0
                     8.0
                     4.0

main = do
    --getConfiguration asg1
    --listSubmissions asg1
    --getSubmission asg1 "12345"
    --createAssignment asg1 cnfg "V:/Univeras/7pusmetis/Haskell/project/PUH_project/Assignment.pdf"
    --upload sub1 (T.pack "some text") "Homework"
    --listFiles sub1
    putStr $ getSubmissionPath sub1



































