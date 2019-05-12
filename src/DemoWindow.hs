module DemoWindow
    (
    ) where

import qualified GI.Handy as Hdy
import qualified GI.Gtk as Gtk
import Prelude

data DemoWindow = DemoWindow
    { parentInstance :: Gtk.ApplicationWindow
    , headerBox :: Hdy.Leaflet
    , contentBox :: Hdy.Leaflet
    , back :: Gtk.Button
    , searchButton :: Gtk.ToggleButton
    , sidebar :: Gtk.StackSidebar
    , stack :: Gtk.Stack
    , boxDialer :: Gtk.Widget
    , dialer :: Hdy.Dialer
    , display :: Gtk.Label
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
    }
