with Ada.Text_IO;

with Glib.Error;

with Gdk.Cairo;
with Gdk.Event;

with Gtk.Builder;
with Gtk.Cell_Renderer_Text;
with Gtk.Editable;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Main;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;

with Cairo;
with Cairo.Image_Surface;

with Aquarius.Buffers;
with Aquarius.Colours;
with Aquarius.Config_Paths;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Sections;
with Aquarius.Sections.Code;
with Aquarius.Trees.Cursors;
with Aquarius.UI.Gtk_Text;

package body Aquarius.UI.Gtk_UI is

   package Main_Window_Callback is
     new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   package Widget_UI_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
        Return_Type => Boolean,
        User_Type   => Gtk_UI_Access);

   package Search_Text_Changed_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.GEntry.Gtk_Entry_Record,
        User_Type   => Gtk_UI_Access);

   package Select_Item_Callback is
     new Gtk.Handlers.User_Callback
       (Widget_Type => Gtk.Tree_View.Gtk_Tree_View_Record,
        User_Type   => Gtk_UI_Access);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   function Configure_Overview_Handler
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      With_UI : Gtk_UI_Access)
      return Boolean;

   function Configure_Section_Area_Handler
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      With_UI : Gtk_UI_Access)
      return Boolean;

   function Draw_Overview_Handler
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      With_UI : Gtk_UI_Access)
      return Boolean;

   function On_Overview_Button_Press
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      With_UI : Gtk_UI_Access)
      return Boolean;

   procedure On_Search_Text_Changed
     (W       : access Gtk.GEntry.Gtk_Entry_Record'Class;
      With_UI : Gtk_UI_Access);

   procedure On_Reference_Activated
     (Widget  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      With_UI : Gtk_UI_Access);

   procedure Render_Overview (With_UI : in out Gtk_UI'Class);
   procedure Render_Sections (With_UI : in out Gtk_UI'Class);

   procedure Update_Identifiers
     (With_UI : in out Gtk_UI'Class);

   --------------------------------
   -- Configure_Overview_Handler --
   --------------------------------

   function Configure_Overview_Handler
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      With_UI : Gtk_UI_Access)
      return Boolean
   is
      Alloc : Gtk.Widget.Gtk_Allocation;
   begin
      W.Get_Allocation (Alloc);
      With_UI.Overview_Height := Natural (Alloc.Height);
      With_UI.Overview_Width := Natural (Alloc.Width);
      W.Queue_Draw;
      return True;
   end Configure_Overview_Handler;

   ------------------------------------
   -- Configure_Section_Area_Handler --
   ------------------------------------

   function Configure_Section_Area_Handler
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      With_UI : Gtk_UI_Access)
      return Boolean
   is
      Alloc : Gtk.Widget.Gtk_Allocation;
   begin
      W.Get_Allocation (Alloc);
      With_UI.Section_Area_Height := Natural (Alloc.Height);
      With_UI.Section_Area_Width := Natural (Alloc.Width);
      return True;
   end Configure_Section_Area_Handler;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   function Draw_Overview_Handler
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      With_UI : Gtk_UI_Access)
      return Boolean
   is
      pragma Unreferenced (W);
   begin
      Render_Overview (With_UI.all);
      return True;
   end Draw_Overview_Handler;

   ----------
   -- Init --
   ----------

   overriding
   procedure Init (With_UI : not null access Gtk_UI) is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Aquarius.Config_Paths.Config_Path & "/gui/aquarius-gtk.ui";
   begin

      Gtk.Main.Init;

      Gtk.Builder.Gtk_New (Builder);

      Ada.Text_IO.Put_Line ("Loading: " & UI_Path);

      declare
         use Glib;
         use type Glib.Error.GError;
         Error : aliased Glib.Error.GError;
         Result : Guint;
      begin
         Result :=
           Gtk.Builder.Add_From_File (Builder, UI_Path, Error'Access);
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & UI_Path;
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      declare
         Main_Window : constant Gtk.Widget.Gtk_Widget :=
                         Gtk.Widget.Gtk_Widget
                           (Builder.Get_Object ("Aquarius_Main_Window"));
      begin
         Main_Window_Callback.Connect
           (Gtk.Window.Gtk_Window (Main_Window),
            "destroy",
            Main_Window_Callback.To_Marshaller (Destroy_Handler'Access));
         Main_Window.Show_All;
      end;

      Aquarius.Sections.Layout.Initialise
        (With_UI.Layout, 10_000, 2_000);

      With_UI.Overview :=
        Gtk.Drawing_Area.Gtk_Drawing_Area
          (Builder.Get_Object ("Overview"));
      declare
         use type Gdk.Event.Gdk_Event_Mask;
      begin
         With_UI.Overview.Add_Events
           (Events =>
              Gdk.Event.Button_Press_Mask or Gdk.Event.Button_Release_Mask
            or Gdk.Event.Button_Motion_Mask);
      end;

      With_UI.Sections :=
        Gtk.Fixed.Gtk_Fixed
          (Builder.Get_Object ("Section_Area"));

      Widget_UI_Callback.Connect
        (With_UI.Overview, Gtk.Widget.Signal_Draw,
         Widget_UI_Callback.To_Marshaller
           (Draw_Overview_Handler'Access),
         With_UI);
      Widget_UI_Callback.Connect
        (With_UI.Overview, "configure-event",
         Widget_UI_Callback.To_Marshaller
           (Configure_Overview_Handler'Access),
         With_UI);
      Widget_UI_Callback.Connect
        (With_UI.Overview, Gtk.Widget.Signal_Button_Press_Event,
         Widget_UI_Callback.To_Marshaller
           (On_Overview_Button_Press'Access),
         With_UI);

      Widget_UI_Callback.Connect
        (With_UI.Sections, "configure-event",
         Widget_UI_Callback.To_Marshaller
           (Configure_Section_Area_Handler'Access),
         With_UI);

      Aquarius.UI.Gtk_Text.Initialise;

      With_UI.Start_X := 0;
      With_UI.Start_Y := 0;

      With_UI.Identifiers :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Identifiers"));

      With_UI.Search :=
        Gtk.GEntry.Gtk_Entry
          (Builder.Get_Object ("Search"));

      Select_Item_Callback.Connect
        (With_UI.Identifiers, Gtk.Tree_View.Signal_Row_Activated,
         Select_Item_Callback.To_Marshaller
           (On_Reference_Activated'Access),
         With_UI);

      Search_Text_Changed_Callback.Connect
        (With_UI.Search, Gtk.Editable.Signal_Changed,
         Search_Text_Changed_Callback.To_Marshaller
           (On_Search_Text_Changed'Access),
         With_UI);

   end Init;

   ------------------------------
   -- On_Overview_Button_Press --
   ------------------------------

   function On_Overview_Button_Press
     (W       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      With_UI : Gtk_UI_Access)
      return Boolean
   is
      pragma Unreferenced (W);
      use type Glib.Guint;
   begin
      if Event.Button.Button = 1 then
         With_UI.Start_X := Natural (Event.Button.X);
         With_UI.Overview.Queue_Draw;
         With_UI.Sections.Queue_Draw;
         return True;
      end if;
      return False;
   end On_Overview_Button_Press;

   ----------------------------
   -- On_Reference_Activated --
   ----------------------------

   procedure On_Reference_Activated
     (Widget  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      With_UI : Gtk_UI_Access)
   is
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
        Widget.Get_Selection;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Model, Iter);
      declare
         Index : constant Natural :=
                   Natural (Gtk.Tree_Model.Get_Int (Model, Iter, 2));
      begin
         if Index > 0 then
            declare
               Program : constant Aquarius.Programs.Program_Tree :=
                           Aquarius.References.Reference_Program
                             (Gtk_UI (With_UI.all).Ref_List (Index));
               Buffer  : constant Aquarius.Buffers.Aquarius_Buffer :=
                           With_UI.Current_Project.Get_Buffer
                             (Program);
               Section : constant Aquarius.Sections.Aquarius_Section :=
                           Aquarius.Sections.Code.New_Code_Section
                             (Program.Show_Location, Buffer);
               Renderer : constant Aquarius.Rendering.Aquarius_Renderer :=
                            Aquarius.Rendering.Sections.New_Section_Renderer
                              (Section);
            begin
               Aquarius.Programs.Arrangements.Arrange
                 (Item        => Program,
                  Line_Length => 40);

               Aquarius.Programs.Arrangements.Render
                 (Program  => Program,
                  Renderer => Renderer,
                  Point    => Aquarius.Trees.Cursors.Left_Of_Tree (Program),
                  Partial  => "");
               With_UI.Show_Section (Section, 200, 0);
               Render_Sections (Gtk_UI (With_UI.all));
            end;
         end if;
      end;
   end On_Reference_Activated;

   ----------------------------
   -- On_Search_Text_Changed --
   ----------------------------

   procedure On_Search_Text_Changed
     (W       : access Gtk.GEntry.Gtk_Entry_Record'Class;
      With_UI : Gtk_UI_Access)
   is
      pragma Unreferenced (W);
   begin
      Update_Identifiers (Gtk_UI (With_UI.all));
   end On_Search_Text_Changed;

   ---------------------
   -- Render_Overview --
   ---------------------

   procedure Render_Overview (With_UI : in out Gtk_UI'Class) is

      use Glib;
      procedure Render (Section       : Aquarius.Sections.Aquarius_Section;
                        X, Y          : Integer);

      Scale : Gdouble;

      Surface : constant Cairo.Cairo_Surface :=
                  Cairo.Image_Surface.Create
                    (Cairo.Image_Surface.Cairo_Format_ARGB32,
                     Gint (With_UI.Overview_Width),
                     Gint (With_UI.Overview_Height));

      Context : Cairo.Cairo_Context :=
                  Cairo.Create (Surface);

      ------------
      -- Render --
      ------------

      procedure Render (Section       : Aquarius.Sections.Aquarius_Section;
                        X, Y          : Integer)
      is
         use Aquarius.Colours;
         use Aquarius.UI.Gtk_Sections;

         Width, Height : Natural;
         Item : constant Gtk_Section :=
                      (if Exists (With_UI.Section_UI, Section.Id)
                       then Get (With_UI.Section_UI, Section.Id)
                       else Create (Section, With_UI.Section_UI));
         Bg       : constant Aquarius_Colour :=
                      Section.Background;

      begin

         if Displayed (Item) then
            Get_Display_Size (Item, Width, Height);

            Cairo.Set_Source_Rgb
              (Context,
               Gdouble (Red (Bg)) / 255.0,
               Gdouble (Green (Bg)) / 255.0,
               Gdouble (Blue (Bg)) / 255.0);
            Cairo.Rectangle (Context,
                             Gdouble (X) * Scale,
                             Gdouble (Y) * Scale,
                             Gdouble (Width) * Scale,
                             Gdouble (Height) * Scale);
            Cairo.Fill (Context);
         end if;
      end Render;

   begin

      declare
         Alloc : Gtk.Widget.Gtk_Allocation;
      begin
         With_UI.Sections.Get_Allocation (Alloc);
         With_UI.Section_Area_Width := Natural (Alloc.Width);
         With_UI.Section_Area_Height := Natural (Alloc.Height);
         Scale := Gdouble (With_UI.Overview_Height) / Gdouble (Alloc.Height);
      end;

      Cairo.Set_Source_Rgb (Context, 0.8, 0.8, 0.8);
      Cairo.Rectangle (Context, 0.0, 0.0,
                       Gdouble (With_UI.Overview_Width),
                       Gdouble (With_UI.Overview_Height));
      Cairo.Fill (Context);

      Cairo.Set_Source_Rgb (Context, 0.9, 0.9, 0.9);
      Cairo.Rectangle (Context,
                       Gdouble (With_UI.Start_X), 1.0,
                       Gdouble (With_UI.Section_Area_Width) * Scale,
                       Gdouble (With_UI.Overview_Height - 2));
      Cairo.Fill (Context);

      Aquarius.Sections.Layout.Render_Overview
        (Layout   => With_UI.Layout,
         Renderer => Render'Access);

      Cairo.Destroy (Context);
      Context := Gdk.Cairo.Create (With_UI.Overview.Get_Window);
      Cairo.Set_Source_Surface (Context, Surface, 0.0, 0.0);
      Cairo.Paint (Context);
      Cairo.Destroy (Context);
      Cairo.Surface_Destroy (Surface);

   end Render_Overview;

   ---------------------
   -- Render_Sections --
   ---------------------

   procedure Render_Sections (With_UI : in out Gtk_UI'Class) is

      procedure Render (Section : Aquarius.Sections.Aquarius_Section;
                        X, Y    : Integer);

      ------------
      -- Render --
      ------------

      procedure Render
        (Section : Aquarius.Sections.Aquarius_Section;
         X, Y    : Integer)
      is
         use Aquarius.UI.Gtk_Sections;
         Item : Gtk_Section;
      begin
         if Exists (With_UI.Section_UI, Section.Id) then
            Item := Get (With_UI.Section_UI, Section.Id);
         else
            Item := Create (Section, With_UI.Section_UI);
         end if;

         Render (Item, With_UI.Sections,
                 X - With_UI.Start_X,
                 Y - With_UI.Start_Y);

      end Render;

   begin
      Aquarius.Sections.Layout.Render
        (Layout   => With_UI.Layout,
         X_Min    => With_UI.Start_X,
         X_Max    => With_UI.Start_X + 1200,
         Y_Min    => With_UI.Start_Y,
         Y_Max    => With_UI.Start_Y + 800,
         Renderer => Render'Access);

      With_UI.Overview.Queue_Draw;

   end Render_Sections;

   ------------------
   -- Show_Project --
   ------------------

   overriding
   procedure Show_Project
     (User_Interface : in out Gtk_UI;
      Project        : not null access
        Aquarius.Projects.Aquarius_Project_Type'Class)
   is
      Model       : Gtk.Tree_Store.Gtk_Tree_Store;
      Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Name_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Loc_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num         : Glib.Gint;
      pragma Warnings (Off, Num);
   begin
      Root_UI_Type (User_Interface).Show_Project (Project);
      User_Interface.Project :=
        Aquarius.Projects.Aquarius_Project (Project);
      Gtk.Tree_Store.Gtk_New (Model,
        (0     => Glib.GType_String,
         1     => Glib.GType_String,
         2     => Glib.GType_Int));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Name_Column);
      Num := User_Interface.Identifiers.Append_Column (Name_Column);
      Gtk.Tree_View_Column.Gtk_New (Loc_Column);
      Num := User_Interface.Identifiers.Append_Column (Loc_Column);
      Name_Column.Pack_Start (Text_Render, True);
      Name_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Name_Column.Add_Attribute (Text_Render, "text", 0);
      Loc_Column.Pack_Start (Text_Render, True);
      Loc_Column.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Loc_Column.Add_Attribute (Text_Render, "text", 1);
      User_Interface.Identifiers.Set_Model (Model.To_Interface);
--        (Gtk.Tree_Model.Gtk_Tree_Model (Model));
      Update_Identifiers (User_Interface);
   end Show_Project;

   ------------------
   -- Show_Section --
   ------------------

   overriding
   procedure Show_Section
     (On      : in out Gtk_UI;
      Section : Aquarius.Sections.Aquarius_Section;
      Hint_X  : Integer;
      Hint_Y  : Integer)
   is
   begin
      Aquarius.Sections.Layout.Show_Section
        (On.Layout, Section, Hint_X, Hint_Y);
   end Show_Section;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (With_UI : in out Gtk_UI) is
   begin

      Render_Sections (With_UI);

      Gtk.Main.Main;

   end Start;

   ------------------------
   -- Update_Identifiers --
   ------------------------

   procedure Update_Identifiers
     (With_UI : in out Gtk_UI'Class)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Model;
      Refs : constant Aquarius.References.Array_Of_Locations :=
               Aquarius.References.Filter
                 (List => With_UI.Project.References,
                  Text => With_UI.Search.Get_Text,
                  Max  => Max_Refs);
      Store : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                Gtk.Tree_Store.Gtk_Tree_Store
                  (-With_UI.Identifiers.Get_Model);
   begin
      Store.Clear;
      for I in Refs'Range loop
         declare
            Parent : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                       Gtk.Tree_Model.Null_Iter;
            Result : Gtk.Tree_Model.Gtk_Tree_Iter;
            Cursor : constant Aquarius.References.Reference_Cursor :=
                       Refs (I);
         begin
            Store.Append (Result, Parent);
            Store.Set (Result, 0,
                       Aquarius.References.Reference_Name (Cursor));
            Store.Set
              (Result, 1,
               Aquarius.References.Reference_Program (Cursor).Name);
            Store.Set (Result, 2, Glib.Gint (I));
         end;
      end loop;

      With_UI.Ref_List (Refs'Range) := Refs;
      With_UI.Ref_Count := Refs'Length;

   end Update_Identifiers;

end Aquarius.UI.Gtk_UI;
