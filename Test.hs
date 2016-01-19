import qualified User        as U

import qualified Assignments as A
import qualified Reviews     as R
import qualified Email       as E

import qualified Data.Text   as T

import Data.List
import Data.Time.Clock

asg1 = A.Assignment 2015 A.Homework 1
asg2 = A.Assignment 2015 A.Homework 2

sub1 = A.Submission asg1 "12345" []
sub2 = A.Submission asg2 "78910" []

asgn = A.Assignment 2016 A.Homework 1

rev = R.ReviewAssignment "Luka124" "3984344" R.Student asgn

re = R.Review rev 9.5 (T.pack "Good job")

cnfg = A.Configuration (read "2011-11-19 18:28:52.607875 UTC" :: UTCTime)
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
    putStr $ A.getSubmissionPath sub1



































