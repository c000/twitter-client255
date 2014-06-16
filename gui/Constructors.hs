module Constructors where

import Imports

constructMainWindow :: IO Window
constructMainWindow = do
    window <- windowNew
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False
    vbox <- vBoxNew False 0
    constructListView          >>= \w -> boxPackStart vbox w PackGrow    0
    constructQuitButton window >>= \w -> boxPackStart vbox w PackNatural 0
    containerAdd window vbox
    return window

constructQuitButton :: Window -> IO Button
constructQuitButton window = do
    b <- buttonNewWithMnemonic "_Quit"
    b `on` buttonActivated $ do
        liftIO mainQuit
    return b

constructListView :: IO ScrolledWindow
constructListView = do
    scroll   <- scrolledWindowNew Nothing Nothing
    store    <- listStoreNew [replicate 100 c ++ "\n" ++ replicate 50 c | c <- ['あ'..'ん']] :: IO (ListStore String)
    cellView <- cellViewNew
    tree     <- treeViewNewWithModel store
    column   <- treeViewColumnNew
    renderer <- cellRendererTextNew
    tree `set` [ treeViewHeadersVisible := True
               , treeViewRulesHint := True
               ]
    column `set` [ treeViewColumnResizable := True
                 , treeViewColumnTitle := "Tweet"
                 ]
    renderer `set` [ cellTextWrapMode := WrapAnywhere ]
    treeViewColumnPackStart column renderer True
    cellLayoutSetAttributes column renderer store $ \row -> [cellText := row]
    treeViewAppendColumn tree column
    containerAdd scroll tree
    return scroll
