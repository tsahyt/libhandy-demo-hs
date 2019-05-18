{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
module DemoWindow
    ( demoWindow
    , appWindow
    ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text)
import Data.Functor (($>))
import Data.Char
import Prelude
import Data.FileEmbed
import Data.GI.Gtk.BuildFn
import GI.Gtk (AttrOp(..), set, on, get, set, after, new)

import qualified GI.Gtk as Gtk
import qualified GI.Handy as Hdy
import qualified GI.Gdk as Gdk
import qualified Data.Text as T
import qualified Data.Text.IO as T

data DemoWindow = DemoWindow
    { appWindow :: Gtk.ApplicationWindow
    , headerBox :: Hdy.Leaflet
    , contentBox :: Hdy.Leaflet
    , back :: Gtk.Button
    , searchButton :: Gtk.ToggleButton
    , sidebar :: Gtk.StackSidebar
    , stack :: Gtk.Stack
    , boxDialer :: Gtk.Box
    , dialer :: Hdy.Dialer
    , display :: Gtk.Label
    , arrows :: Hdy.Arrows
    , searchBar :: Hdy.SearchBar
    , searchEntry :: Gtk.SearchEntry
    , arrowsListbox :: Gtk.ListBox
    , arrowsDirectionRow :: Hdy.ComboRow
    , columnListbox :: Gtk.ListBox
    , listsListbox :: Gtk.ListBox
    , comboRow :: Hdy.ComboRow
    , enumComboRow :: Hdy.ComboRow
    , headerGroup :: Hdy.HeaderGroup
    , adjArrowsCount :: Gtk.Adjustment
    , adjArrowsDuration :: Gtk.Adjustment
    , presentationDialogButton :: Gtk.Button
    , actionDialogButton :: Gtk.Button
    }

keyPressedCB :: DemoWindow -> Gdk.EventKey -> IO Bool
keyPressedCB win key = do
    kv <- get key #keyval
    st <- get key #state
    if (kv == Gdk.KEY_q || kv == Gdk.KEY_Q) &&
       st == [Gdk.ModifierTypeControlMask]
        then Gtk.widgetDestroy (appWindow win) $> True
        else pure False

update :: DemoWindow -> IO ()
update DemoWindow {..} = do
    headerChild <-
        do w <- get headerBox #visibleChild
           Gdk.castTo Gtk.HeaderBar w
    fold <- get headerBox #fold
    -- don't use the overloaded function here because it is not nullable!
    Hdy.headerGroupSetFocus headerGroup $
        case fold of
            Hdy.FoldFolded -> headerChild
            Hdy.FoldUnfolded -> Nothing

updateHeaderBar :: DemoWindow -> IO ()
updateHeaderBar DemoWindow {..} = do
    visibleChildName <- get stack #visibleChildName
    set searchButton [#visible := visibleChildName == Just "search-bar"]

demoWindowUI :: Text
demoWindowUI = $(embedStringFile "res/hdy-demo-window.ui")

buildDemoWindow :: BuildFn DemoWindow
buildDemoWindow = DemoWindow
    <$> getObject Gtk.ApplicationWindow "app_window"
    <*> getObject Hdy.Leaflet "header_box"
    <*> getObject Hdy.Leaflet "content_box"
    <*> getObject Gtk.Button "back"
    <*> getObject Gtk.ToggleButton "search_button"
    <*> getObject Gtk.StackSidebar "sidebar"
    <*> getObject Gtk.Stack "stack"
    <*> getObject Gtk.Box "box_dialer"
    <*> getObject Hdy.Dialer "dialer"
    <*> getObject Gtk.Label "display"
    <*> getObject Hdy.Arrows "arrows"
    <*> getObject Hdy.SearchBar "search_bar"
    <*> getObject Gtk.SearchEntry "search_entry"
    <*> getObject Gtk.ListBox "arrows_listbox"
    <*> getObject Hdy.ComboRow "arrows_direction_row"
    <*> getObject Gtk.ListBox "column_listbox"
    <*> getObject Gtk.ListBox "lists_listbox"
    <*> getObject Hdy.ComboRow "combo_row"
    <*> getObject Hdy.ComboRow "enum_combo_row"
    <*> getObject Hdy.HeaderGroup "header_group"
    <*> getObject Gtk.Adjustment "adj_arrows_count"
    <*> getObject Gtk.Adjustment "adj_arrows_duration"
    <*> getObject Gtk.Button "presentation_dialog_button"
    <*> getObject Gtk.Button "action_dialog_button"

dialerSignals :: MonadIO m => DemoWindow -> m ()
dialerSignals DemoWindow{..} = do
    after dialer #submitted $ \number -> T.putStrLn ("Submit " <> number)
    after dialer #deleted $ T.putStrLn "Delete btn"
    after dialer #symbolClicked $ \o ->
        T.putStrLn (T.snoc "clicked: " (chr (fromIntegral o)))
    on dialer
        (Gdk.PropertyNotify #number)
        (\_ -> get dialer #number >>= \num -> set display [#label := num])
    
    pure ()

dialogLabel :: IO Gtk.Label
dialogLabel =
    new Gtk.Label
        [ #label := "Hello, World!"
        , #vexpand := True
        , #valign := Gtk.AlignCenter
        , #halign := Gtk.AlignCenter
        ]

presentationDialog :: DemoWindow -> IO ()
presentationDialog DemoWindow{..} = do
    dlg <- new Hdy.Dialog [#title := "HdyDialog", #transientFor := appWindow ]
    lbl <- dialogLabel

    Gtk.dialogGetContentArea dlg >>= flip Gtk.containerAdd lbl
    Gtk.widgetShow lbl
    Gtk.widgetShow dlg

actionDialog :: DemoWindow -> IO ()
actionDialog DemoWindow {..} = do
    dlg <-
        new
            Hdy.Dialog
            [ #title := "HdyDialog"
            , #transientFor := appWindow
            , #useHeaderBar := 1
            ]
    Gtk.dialogAddButton
        dlg
        "Done"
        (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
    Gtk.dialogAddButton
        dlg
        "Cancel"
        (fromIntegral . fromEnum $ Gtk.ResponseTypeCancel)
    Gtk.dialogSetDefaultResponse
        dlg
        (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
    on dlg #response $ const (Gtk.widgetDestroy dlg)
    lbl <- dialogLabel
    Gtk.dialogGetContentArea dlg >>= flip Gtk.containerAdd lbl
    Gtk.widgetShow lbl
    Gtk.widgetShow dlg

demoWindow :: MonadIO m => Gtk.Application -> m DemoWindow
demoWindow app = do
    b <- Gtk.builderNewFromString demoWindowUI (-1)
    w@DemoWindow {..} <- buildWithBuilder buildDemoWindow b
    -- set application
    set appWindow [#application := app]
    set arrowsDirectionRow []
    -- signals
    on appWindow #keyPressEvent (keyPressedCB w)
    on headerBox (Gdk.PropertyNotify #visibleChild) (\_ -> update w)
    on headerBox (Gdk.PropertyNotify #fold) (\_ -> update w)
    on stack
        (Gdk.PropertyNotify #visibleChild)
        (\_ ->
             set contentBox [#visibleChildName := "content"] *>
             updateHeaderBar w)
    on back #clicked $ set contentBox [#visibleChildName := "sidebar"]

    dialerSignals w

    {- on arrowsDirectionRow (Gdk.PropertyNotify #selectedIndex) $ \_ ->
     -     set arrows [#direction := undefined] -}
    on adjArrowsCount #valueChanged $
        set arrows [ #count :=> truncate <$> get adjArrowsCount #value ]
    on adjArrowsDuration #valueChanged $
        set arrows [ #duration :=> truncate <$> get adjArrowsDuration #value ]
    
    on presentationDialogButton #clicked (presentationDialog w)
    on actionDialogButton #clicked (actionDialog w)

    pure w
