--  Aquarius.UI.Gtk_View
--  Top-level GtkAda frontend presenting the code-bubbles layout:
--  a miniview strip along the top, a structure tree on the right and
--  a large bubble canvas filling the remaining space.

package Aquarius.UI.Gtk_View is

   procedure Launch;
   --  Initialise Gtk, build the main window and run the Gtk main loop.
   --  Returns when the window is closed.

end Aquarius.UI.Gtk_View;
