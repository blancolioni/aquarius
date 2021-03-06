with Ada.Text_IO;

with Glib.Error;
with Glib.Main;

with Gdk.Pixbuf;

with Gtk.Builder;
with Gtk.Main;
with Gtk.Window;

with Aquarius.Configuration;

--  with Aquarius.Bubbles.Notes;
--  with Aquarius.Bubbles.Collections;

with Aquarius.GUI.Main;
with Aquarius.GUI.Manager;
with Aquarius.GUI.Menu;
with Aquarius.GUI.Message_View;
with Aquarius.GUI.Source;
with Aquarius.GUI.Text;
with Aquarius.GUI.Views;

with Aquarius.Fragments.Notes;

with Aquarius.Buffers;
with Aquarius.Tasks;

package body Aquarius.GUI is

   Local_Current_Project  : Aquarius.Projects.Aquarius_Project;
   Local_Current_UI       : constant access Aquarius.UI.Aquarius_UI'Class :=
     new Gtk_UI;

   --  Local_Bubble_Collection :
   --  Bubbles.Collections.Aquarius_Bubble_Collection;

   Timeout_Id : Glib.Main.G_Source_Id;
   pragma Warnings (Off, Timeout_Id);

   function Timeout return Boolean;

   ---------------------
   -- Current_Project --
   ---------------------

   function Current_Project
     return Aquarius.Projects.Aquarius_Project
   is
   begin
      return Local_Current_Project;
   end Current_Project;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI
     return access Aquarius.UI.Aquarius_UI'Class
   is
   begin
      return Local_Current_UI;
   end Current_UI;

   ----------------
   -- Launch_GUI --
   ----------------

   procedure Launch_GUI
     (With_File    : String := "";
      With_Project : Aquarius.Projects.Aquarius_Project := null)
   is
      use type Aquarius.Projects.Aquarius_Project;
      Builder : Gtk.Builder.Gtk_Builder;
      Builder_File_Path : constant String :=
                            Aquarius.Configuration.Get_Library_Path &
                            "/Aquarius.UI";
   begin

      Local_Current_Project := With_Project;

      --  Aquarius.Bubbles.Collections.Add_Bubble
      --    (To     => Local_Bubble_Collection,
      --     Bubble => Aquarius.Bubbles.Notes.Create_Note);

      --  Gtk.Main.Set_Locale;
      Gtk.Main.Init;
      Gtk.Builder.Gtk_New (Builder);

      Ada.Text_IO.Put_Line ("Loading: " & Builder_File_Path);

      declare
         use type Glib.Error.GError;
         use type Glib.Guint;
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => Builder_File_Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & Builder_File_Path;
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      if True then
         Aquarius.GUI.Main.Initialise (Builder);
         Aquarius.GUI.Menu.Initialise (Builder);
         Aquarius.GUI.Message_View.Initialise (Builder);
         Aquarius.GUI.Source.Initialise (Builder);
      end if;

      Aquarius.GUI.Text.Initialise;
      --  Aquarius.GUI.Manager.Initialise (Builder);
      Aquarius.GUI.Views.Initialise (Builder);

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object
                              ("Aquarius_Main_Window"));
         Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf;
         Error       : Glib.Error.GError;
      begin

         Gdk.Pixbuf.Gdk_New_From_File
           (Pixbuf,
            Aquarius.Configuration.Get_Library_Path & "/images/aquarius.png",
            Error);
         Main_Window.Set_Icon (Pixbuf);
         Main_Window.Show_All;
      end;

      declare
         use Aquarius.Projects;
      begin
         if Local_Current_Project = null then
            Local_Current_Project :=
              Aquarius.Projects.New_Default_Project
                (With_File,
                 Local_Current_UI);
         end if;

         Aquarius.GUI.Views.Update_Views (Local_Current_Project);
         Aquarius.GUI.Source.Load_Buffer
           (Local_Current_Project.Get_Main_Buffer);
      end;

--        if With_File /= "" then
--           Aquarius.GUI.Source.Load_File (With_File);
--        end if;

      if False then
         declare
            F : constant Aquarius.Fragments.Aquarius_Fragment :=
                  Aquarius.Fragments.Notes.Create_Note
                    (500, 100,
                     "Welcome to Aquarius!" & Character'Val (10)
                     & "Double-click an entity to the right to start");
         begin

            Aquarius.GUI.Manager.Add_Fragment (F, 20, 20);

         end;
      end if;

      Timeout_Id := Glib.Main.Timeout_Add (Interval => 100,
                                           Func     => Timeout'Access);

      Gtk.Main.Main;

   end Launch_GUI;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Gtk_UI) return String is
      pragma Unreferenced (Item);
   begin
      return "Aquarius-Gtk";
   end Name;

   ---------------------
   -- Show_Interactor --
   ---------------------

   overriding
   procedure Show_Interactor
     (UI    : access Gtk_UI;
      Item  : access Aquarius.Interaction.Interactor'Class)
   is
      pragma Unreferenced (UI);
   begin
      Aquarius.GUI.Source.Load_Buffer
        (Aquarius.Buffers.Aquarius_Buffer (Item));
   end Show_Interactor;

   -------------
   -- Timeout --
   -------------

   function Timeout return Boolean is
   begin
      if Aquarius.Tasks.Changed ("GUI") then
         Aquarius.Tasks.Clear_Changed ("GUI");
         Aquarius.GUI.Views.Update_Views (Local_Current_Project);
         Ada.Text_IO.Put_Line ("Updating GUI");
      end if;
      return True;
   end Timeout;

   -------------------------
   -- Update_Message_View --
   -------------------------

   overriding
   procedure Update_Message_View (Item : in out Gtk_UI) is
      pragma Unreferenced (Item);
   begin
      Aquarius.GUI.Message_View.Update;
   end Update_Message_View;

end Aquarius.GUI;
