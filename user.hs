{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module User (
    User(User),
    createUser,
    updateUser,
    deleteUser,
    listUsers,
    listUsersInRole,
    getUser,
    identifier,
    email,
    isRoleInYear,
    UserIdentifier) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Role
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B

type UserIdentifier = String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    identifier  UserIdentifier
    email       String
    pwdHash     String
    role        Role
    UUID        identifier
|]

instance Show User where
    show u = "{identifier = " ++ show (userIdentifier u) ++
             ",email = " ++ show (userEmail u) ++
             ",role = " ++ show (userRole u) ++ "}"
             
identifier :: User -> UserIdentifier
identifier = userIdentifier

email :: User -> String
email = userEmail

-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate
-- | exception.
createUser :: UserIdentifier -> String -> String -> Role -> IO User
createUser id email pwd role = do
    hash <- makePassword (B.pack pwd) 12
    runSqlite "UserDB.db" $ do
        runMigration migrateAll
        userId <- insert $ User id email (B.unpack hash) role
        maybeUser <- get userId
        case maybeUser of
            Nothing -> error "Failed to add user"
            Just user -> return user

-- | Updates a given user. Identifies it by the UserIdentifier (or
-- | maybe database id field, if you added it) in the User and overwrites
-- | the DB entry with the values in the User structure. Throws an
-- | appropriate error if it cannot do so; e.g. the user does not exist.
updateUser :: User -> IO ()
updateUser u = do
    runSqlite "UserDB.db" $ do
        runMigration migrateAll
        maybeUser <- getBy $ UUID $ userIdentifier u
        case maybeUser of
            Nothing -> error "User not found"
            Just (Entity userId user) -> replace userId u
        return ()    
        
-- | Deletes a user referenced by identifier. If no such user or the
-- | operation fails, an appropriate exception is thrown.
deleteUser :: UserIdentifier -> IO ()
deleteUser id = do 
    runSqlite "UserDB.db" $ do
        runMigration migrateAll
        maybeEntity <- getBy $ UUID id
        case maybeEntity of
            Nothing -> error "User not found"
            Just entity -> deleteBy $ UUID id
        return ()
 
-- | Lists all the users
listUsers :: IO [User] 
listUsers = runSqlite "UserDB.db" $ do 
    dbList <- selectList [] []
    let aa = Prelude.map entityVal dbList
    return (aa)
    
-- | Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole role = do
    runSqlite "UserDB.db" $ do 
        dbList <- selectList [UserRole ==. role] []
        let aa = Prelude.map entityVal dbList
        return (aa)
    
-- | Fetches a single user by identifier
getUser :: UserIdentifier -> IO User 
getUser id = do
    runSqlite "UserDB.db" $ do
        runMigration migrateAll
        maybeEntity <- getBy $ UUID id
        case maybeEntity of
            Nothing -> error "User not found"
            Just entity -> return $ entityVal entity


-- | Checks whether the user has a role of AT LEAST X in a given academic
-- | year.
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear ( User _ _ _ (Professor)) Professor _ = True
isRoleInYear ( User _ _ _ (Student y)) (Student _) y1 = 
    if (y1-1 == y || y1 == y) then True else False
isRoleInYear ( User _ _ _ (TA y0 y1)) (TA _ _) y = 
    if (y0 <= y && y1 >= y) then True else False
isRoleInYear _ _ _ = False 