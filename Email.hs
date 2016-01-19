module Email (
    Template,
    Configuration,
    compileTemplate,
    readConfig,
    sendMail) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Network.Mail.SMTP as Mail
import qualified Data.Text.Lazy as L
import qualified Network.Mail.Mime as Mime
import Data.Word
import Network.Socket.Internal
import Data.List

-- | An alias for the template contents 
type Template = T.Text

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

data Role = Student Integer -- Academic Year shorthand (2015 for 2015/16)
    | TA Integer Integer -- AY shorthand range (inclusive)
    | Professor deriving (Eq, Ord, Show)

-- | A user (the definition can be bigger)
data User = User { identifier :: UserIdentifier
    , email :: String
    , pwdHash :: String
    , role :: Role
    } deriving (Eq, Show)

user1 = User {identifier = "Vienas", email = "medinismolis@gmx.com", pwdHash = "sgsvht", role = Professor};
user2 = User {identifier = "Du", email = "medinismolis@gmx.com", pwdHash = "sgsvht", role = Student {}};
user3 = User {identifier = "Trys", email = "medinismolis@gmx.com", pwdHash = "sgsvht", role = Student {}};
user4 = User {identifier = "Keturi", email = "medinismolis@gmx.com", pwdHash = "sgsvht", role = TA {}};
user5 = User {identifier = "Linas", email = "medinismolis@gmx.com", pwdHash = "sgsvht", role = TA {}};
userList = [user1, user2, user3, user4, user5]

-- | Configuration object
data Configuration = Configuration { 
    host       :: String,
    port       :: PortNumber,
    emailAddr  :: String,
    username   :: String,
    password   :: String,
    ident      :: UserIdentifier
} deriving (Show)

type Error = String

-- | Parses an expression using traslateTags
compileTemplate :: Template -> M.Map String String -> Either Error L.Text
compileTemplate tmpl tmap = translateTags tmpl tmap

-- | Get template text from a file path
getTemplate :: FilePath -> IO T.Text
getTemplate fp = do
    contents <- readFile fp
    return $ T.pack contents

-- | Get configuration from a file path
getConfiguration :: FilePath -> IO Configuration
getConfiguration fp = do
    contents <- readFile fp
    return $ confFromStr contents

confFromStr :: String -> Configuration
confFromStr str = Configuration {
      host = drop 2 $ dropWhile (/= '=') (lns !! 0),
      port = toEnum (read (drop 2 $ dropWhile (/= '=') (lns !! 1)) :: Int),
      emailAddr = drop 2 $ dropWhile (/= '=') (lns !! 2),
      username = drop 2 $ dropWhile (/= '=') (lns !! 3),
      password = drop 2 $ dropWhile (/= '=') (lns !! 4),
      ident = drop 2 $ dropWhile (/= '=') (lns !! 5)
   }
   where lns = lines str

-- | Sends an e-mail with given text to list of users 
-- | using given configuration. Throws appropriate error upon failure. 
-- | Provide subject of the email as the last argument.
sendMail :: IO Configuration -> IO T.Text -> [User] -> String -> IO ()
sendMail iocnfg iotxt list subj = do
    txt <- iotxt
    cnfg <- iocnfg
    let mails = map (\x -> (T.pack $ email x, unwrapEither $ compileTemplate txt (createTemplateMap (ident cnfg) x), identifier x)) list
    mapM_ (\(x,y,z) -> sendSingleMail x y z cnfg subj) mails -- sends all emails
    --mapM_ (\(x,y,_) -> putStrLn $ (T.unpack x ++ " -> \n" ++ L.unpack y)) mails -- prints all emails
    
-- | Prepares and sends email
sendSingleMail :: T.Text -> L.Text -> String -> Configuration -> String -> IO ()
sendSingleMail addr txt recipName cnfg subj = do
    let from = Mime.Address (Just $ T.pack $ ident cnfg) (T.pack $ emailAddr cnfg)
    let to = [Mime.Address (Just $ T.pack recipName) (addr)]
    let cc = []
    let bcc = []
    let subject = T.pack subj
    let plainTextPart = Mail.plainTextPart txt
    let parts = [plainTextPart]
    let simpleMail = Mail.simpleMail from to cc bcc subject parts
    Mail.sendMailWithLogin' (host cnfg) (port cnfg) (username cnfg) (password cnfg) simpleMail


-- | Available tags and booleans have their values stored in a
-- | Data.Map and are looked up when needed
createTemplateMap :: UserIdentifier -> User -> M.Map String String
createTemplateMap authorName user = M.fromList 
    [("<<AuthorName>>", authorName)
    ,("<<UserName>>", identifier user)
    ,("isStudent", isStudent user)
    ,("isTA", isTA user)
    ,("isProf", isProf user)
    ,("True", "True")
    ,("False", "False")
    ]


-- | Traverses the text line by line and word by word to find tags
-- | and <<If>> statements. 
translateTags :: T.Text -> M.Map String String -> Either Error L.Text
translateTags txt tmap = Right ( L.pack $ translateTagsLines (lines $ T.unpack txt) [])
    where translateTagsLines [] t = unlines t
          translateTagsLines (x:xs) t = if isInfixOf "<<If>>" x
                                        then if checkCondition (head xs) tmap
                                             then translateTagsLines (drop 2 $ dropWhile (\l -> not $ isInfixOf "<</If>>" l) xs) (t ++ [translateTagsLines (drop 2 $ takeWhile (\l -> not $ isInfixOf "<<Else>>" l) xs) []])
                                             else translateTagsLines (drop 2 $ dropWhile (\l -> not $ isInfixOf "<</If>>" l) xs) (t ++ [translateTagsLines (drop 1 $ dropWhile (\l -> not $ isInfixOf "<<Else>>" l) $ takeWhile (\l -> not $ isInfixOf "<</If>>" l) xs) []])
                                        else translateTagsLines xs (t ++ [translateTagsWords (words x) []])
          translateTagsWords [] t = unwords t
          translateTagsWords (x:xs) t = translateTagsWords xs (t ++ [checkWord x])
          checkWord word = if (length elems) > 0
                           then (prefx ++ changedWord ++ sufx)
                           else word
                           where elems = M.elems (M.filterWithKey (\k _ -> isInfixOf k word) tmap)
                                 changedWord = unwords elems
                                 prefx = takeWhile (\l -> l /= '<') word
                                 sufx  = drop 2 $ dropWhile (\l -> l /= '>') word


data Expr = And Expr Expr | Or Expr Expr | Not Expr | Val Bool
    deriving (Show,Read)

eval' :: Expr -> Bool
eval' (Val x)   = x
eval' (Not x)   = not $ eval' x
eval' (Or x y)  = eval' x || eval' y
eval' (And x y) = eval' x && eval' y

addSpaces :: String -> String
addSpaces [] = ""
addSpaces ('(':xs) = "( " ++ addSpaces xs
addSpaces (')':xs) = " )" ++ addSpaces xs
addSpaces (x:xs)   = [x] ++ addSpaces xs

checkCondition :: String -> M.Map String String -> Bool
checkCondition x tmap = eval' (read (cc (words (addSpaces x)) "") :: Expr)
    where cc [] exp = exp
          cc (x:xs) exp = if M.member x tmap
                          then cc xs (exp ++ "Val " ++ unwrapMaybe(M.lookup x tmap))
                          else cc xs (exp ++ x)

isStudent :: User -> String
isStudent (User _ _ _ (Student _)) = "True"
isStudent (User _ _ _ _) = "False"

isTA :: User -> String
isTA (User _ _ _ (TA _ _)) = "True"
isTA (User _ _ _ _) = "False"

isProf :: User -> String
isProf (User _ _ _ Professor) = "True"
isProf (User _ _ _ _) = "False"

unwrapMaybe :: Maybe String -> String
unwrapMaybe a = case a of
    Just a -> a
    Nothing -> ""

unwrapEither :: Either a b -> b
unwrapEither a = case a of
    Right x -> x
    Left err -> error "err"