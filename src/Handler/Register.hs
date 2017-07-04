{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Register where

import Import
import Yesod.Form.Bootstrap3

data Person = Person
  { name :: Text
  , mobileNumber :: Int
  , email :: Maybe Text
  , password :: Text
  }
  deriving Show

personForm :: AForm Handler Person
personForm = Person
  <$> areq textField (bfs ("Your name" :: Text)) Nothing
  <*> areq intField (bfs ("Mobile number" :: Text)) Nothing
  <*> aopt textField (bfs ("Email address (optional)" :: Text)) Nothing
  <*> areq passwordField (bfs ("Password" :: Text)) Nothing

getRegisterR :: Handler Html
getRegisterR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm personForm
  defaultLayout $ do
    $(widgetFile "register/register")

postRegisterR :: Handler Html
postRegisterR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm personForm
  case res of
    FormSuccess person -> defaultLayout [whamlet|<p>#{show person} |]
    _ -> defaultLayout $(widgetFile "register/register")
