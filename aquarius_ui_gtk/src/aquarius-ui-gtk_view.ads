--  Aquarius.UI.Gtk_View
--  Top-level GtkAda frontend presenting the code-bubbles layout:
--  a miniview strip along the top and a large bubble canvas filling the
--  remaining space. The structure tree is presented as a fixed bubble on
--  the canvas rather than a separate panel.

package Aquarius.UI.Gtk_View is

   procedure Launch (Icon_Dir : String := "");
   --  Initialise Gtk, build the main window and run the Gtk main loop.
   --  Returns when the window is closed.
   --  Icon_Dir, if non-empty, is a directory containing the window-icon
   --  images "aquarius-<size>.png" (16, 24, 32, 48, 256). They are supplied
   --  to Gtk as an icon list so the title bar and Windows taskbar each pick
   --  a natively-rendered size rather than downscaling one large image.
   --  Missing or unreadable files are skipped; loading the icons is
   --  non-fatal.

end Aquarius.UI.Gtk_View;
