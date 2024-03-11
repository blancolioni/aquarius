with Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.String_Maps;
with WL.String_Sets;

with Ack.Bindings.Actions;
with Ack.Classes;
with Ack.Compile;
with Ack.Errors;
with Ack.Features;
with Ack.Semantic;

with Aquarius.Configuration;
with Aquarius.Syntax;
with Aquarius.Trees;

package body Ack.Bindings is

   Report_Calls          : constant Boolean := False;
--     Report_Implicit_Calls : constant Boolean := False;
   Report_Class_Load     : constant Boolean := False;

   package Link_Name_To_Class_Maps is
     new WL.String_Maps
       (Ack.Classes.Constant_Class_Entity, Ack.Classes."=");

   type Implicit_Call_Record is
      record
         Feature_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Implicit_Call_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Implicit_Call_Record);

   Local_Program_Tree_Class : Ack.Classes.Constant_Class_Entity;

   function Aquarius_Trees_Program_Tree
     return Ack.Classes.Constant_Class_Entity;

   type Binding_Record is
      record
         Parent_Tree          : Ada.Strings.Unbounded.Unbounded_String;
         Child_Tree           : Ada.Strings.Unbounded.Unbounded_String;
         Parent_Full_Name     : Ada.Strings.Unbounded.Unbounded_String;
         Child_Full_Name      : Ada.Strings.Unbounded.Unbounded_String;
         Child_Type           : Ada.Strings.Unbounded.Unbounded_String;
         Parent_Class         : Ack.Classes.Constant_Class_Entity;
         Child_Class          : Ack.Classes.Constant_Class_Entity;
         Position             : Binding_Position;
         References           : List_Of_Constant_Entities.List;
         Implicit_Calls       : Implicit_Call_Lists.List;
         Has_Feature_Binding  : Boolean;
      end record;

   package Binding_Record_Vectors is
     new Ada.Containers.Vectors (Positive, Binding_Record);

   ---------------------------------
   -- Aquarius_Trees_Program_Tree --
   ---------------------------------

   function Aquarius_Trees_Program_Tree
     return Ack.Classes.Constant_Class_Entity
   is
      use type Ack.Classes.Constant_Class_Entity;
   begin
      if Local_Program_Tree_Class = null then
         Local_Program_Tree_Class :=
           Ack.Semantic.Get_Class ("Aquarius.Trees.Program_Tree");
      end if;
      return Local_Program_Tree_Class;
   end Aquarius_Trees_Program_Tree;

   ----------------------
   -- Load_Ack_Binding --
   ----------------------

   function Load_Ack_Binding
     (Binding_File_Path : String;
      Base_Aqua_Path    : String;
      Grammar           : Aquarius.Grammars.Aquarius_Grammar;
      Group             : Aquarius.Actions.Action_Group)
      return Boolean
   is
      References     : List_Of_Constant_Entities.List;
      Binding_Table  : Ack.Bindings.Actions.Ack_Binding_Table;
      Binding_Vector : Binding_Record_Vectors.Vector;
      Local_Classes  : Link_Name_To_Class_Maps.Map;
      Aqua_Bound_Classes : WL.String_Sets.Set;

      Group_Name     : constant String :=
                         Aquarius.Actions.Action_Group_Name (Group);

      First_Class : Boolean := True;
      Newest_Class_Time : Ada.Calendar.Time;

      function Binding_Name (Index : Positive) return String;

      function To_Source_File_Name
        (Class_Name : String)
         return String;

      function Position_Name (Position : Binding_Position) return String
      is (case Position is
             when Before => "Before",
             when After  => "After");

      function Is_Group_Reference
        (Class    : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Property : not null access constant Root_Entity_Type'Class)
         return Boolean;

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Load_Object_File
        (Feature_Name : String;
         Binding      : Binding_Record);

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature      : not null access constant
           Ack.Root_Entity_Type'Class);

      procedure Check_Bindings
        (Tree_Name : String);

      procedure Check_Conforming_Children
        (Parent_Name : String;
         Child_Name  : String;
         Calls       : in out Implicit_Call_Lists.List);

      procedure Write_Aqua_Binding_Class;

      -------------------------
      -- Add_Feature_Binding --
      -------------------------

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature      : not null access constant
           Ack.Root_Entity_Type'Class)
      is
         Feature_Name : constant String := Feature.Standard_Name;
         Index         : constant Natural :=
                           Ada.Strings.Fixed.Index (Feature_Name, "_");
         Position_Name : constant String :=
                           (if Index > 0
                            then Feature_Name (Feature_Name'First .. Index - 1)
                            else "");
         Position      : constant Binding_Position :=
                           (if Position_Name = "after"
                            then After
                            else Before);
         Child_Tree    : constant String :=
                           Feature_Name (Index + 1 .. Feature_Name'Last);
         Parent_Tree   : constant String := Class.Standard_Name;
      begin

         if not Grammar.Have_Syntax (Parent_Tree) then
            return;
         end if;

         if not Local_Classes.Contains (Class.Standard_Name) then
            Local_Classes.Insert
              (Class.Standard_Name,
               Ack.Classes.Constant_Class_Entity (Class));
         end if;

         if Ack.Features.Is_Feature (Feature)
           and then Ack.Features.Feature_Entity_Record (Feature.all)
           .Is_Property
           and then Is_Group_Reference
             (Class,
              Ack.Features.Feature_Entity_Record'Class (Feature.all)'Access)
         then
            References.Append (Constant_Entity_Type (Feature));
            return;
         end if;

         if Position_Name = ""
           or else not Grammar.Have_Syntax (Parent_Tree)
           or else (Child_Tree /= "" and then Child_Tree /= "node"
                    and then not Grammar.Have_Syntax (Child_Tree))
         then
            return;
         end if;

         if Index > 0
           and then (Position_Name = "before"
                     or else Position_Name = "after")
         then

            declare
               Rec : Binding_Record :=
                       Binding_Record'
                         (Parent_Tree          => +Parent_Tree,
                          Child_Tree           => <>,
                          Parent_Full_Name     => +Class.Qualified_Name,
                          Child_Full_Name      => <>,
                          Child_Type           => <>,
                          Parent_Class         =>
                            Ack.Classes.Constant_Class_Entity (Class),
                          Child_Class          => <>,
                          Position             => Position,
                          Has_Feature_Binding  => True,
                          References           => References,
                          Implicit_Calls       => <>);
            begin

               Ack.Bindings.Actions.Create_Binding
                 (Table            => Binding_Table,
                  Parent_Tree_Name => Parent_Tree,
                  Child_Tree_Name  => Child_Tree,
                  Position         => Position);

               if Child_Tree /= "node" then
                  declare
                     Child_Argument : constant Entity_Type :=
                                        Feature.Argument (1);
                     Child_Type     : constant Entity_Type :=
                                        Child_Argument.Get_Type;
                  begin
                     Rec.Child_Tree := +Child_Tree;
                     Rec.Child_Class :=
                       Ack.Classes.Constant_Class_Entity
                         (Child_Type.Class_Context);

                     if not Local_Classes.Contains (Child_Tree) then
                        Local_Classes.Insert (Child_Tree, Rec.Child_Class);
                     end if;

                     Rec.Child_Full_Name := +Rec.Child_Class.Qualified_Name;

                     if Position = Before then
                        Check_Conforming_Children
                          (Parent_Tree, Child_Tree,
                           Rec.Implicit_Calls);
                     end if;
                  end;
               end if;

               Binding_Vector.Append (Rec);
            end;
         end if;
      end Add_Feature_Binding;

      ------------------
      -- Binding_Name --
      ------------------

      function Binding_Name (Index : Positive) return String is
         use type Ada.Strings.Unbounded.Unbounded_String;
         Rec : constant Binding_Record := Binding_Vector.Element (Index);
      begin
         return -Rec.Parent_Tree & "_" & Position_Name (Rec.Position)
           & (if Rec.Child_Tree = "" then ""
              else "_" & (-Rec.Child_Tree));
      end Binding_Name;

      --------------------
      -- Check_Bindings --
      --------------------

      procedure Check_Bindings
        (Tree_Name : String)
      is
         Tree     : constant Aquarius.Syntax.Syntax_Tree :=
                      Grammar.Get_Syntax (Tree_Name);
         Children : constant Aquarius.Trees.Array_Of_Trees :=
                      Tree.Get_Named_Children;

         procedure Check_Child_Binding
           (Syntax : Aquarius.Syntax.Syntax_Tree);

         -------------------------
         -- Check_Child_Binding --
         -------------------------

         procedure Check_Child_Binding
           (Syntax : Aquarius.Syntax.Syntax_Tree)
         is
            use all type Aquarius.Syntax.Node_Class;
            Child_Name : constant String := Syntax.Name;
            Calls    : Implicit_Call_Lists.List;
            Parent   : constant Ack.Classes.Constant_Class_Entity :=
                         Local_Classes.Element (Tree_Name);
            Child    : Ack.Classes.Constant_Class_Entity;
         begin
            if Syntax.Syntax_Class in Choice | Non_Terminal
              and then Local_Classes.Contains (Child_Name)
            then
               Check_Conforming_Children
                 (Tree_Name, Child_Name, Calls);

               if not Calls.Is_Empty then

                  Child := Local_Classes.Element (Child_Name);

                  Binding_Vector.Append
                    (Binding_Record'
                       (Parent_Tree         => +Tree_Name,
                        Child_Tree          => +Child_Name,
                        Parent_Full_Name    => +Parent.Qualified_Name,
                        Child_Full_Name     => +Child.Qualified_Name,
                        Child_Type          => <>,
                        Parent_Class        => Parent,
                        Child_Class         => Child,
                        Position            => Before,
                        Implicit_Calls      => Calls,
                        References          => <>,
                        Has_Feature_Binding => False));
               end if;
            end if;
         end Check_Child_Binding;

      begin
         if Local_Classes.Contains (Tree_Name) then
            for Child of Children loop
               if not Ack.Bindings.Actions.Have_Binding
                 (Table            => Binding_Table,
                  Parent_Tree_Name => Tree_Name,
                  Child_Tree_Name  => Child.Name,
                  Position         => Before)
               then
                  Ack.Bindings.Actions.Create_Binding
                    (Table            => Binding_Table,
                     Parent_Tree_Name => Tree_Name,
                     Child_Tree_Name  => Child.Name,
                     Position         => Before);

                  Check_Child_Binding (Aquarius.Syntax.Syntax_Tree (Child));
               end if;
            end loop;

         end if;
      end Check_Bindings;

      -------------------------------
      -- Check_Conforming_Children --
      -------------------------------

      procedure Check_Conforming_Children
        (Parent_Name : String;
         Child_Name  : String;
         Calls       : in out Implicit_Call_Lists.List)
      is
         use Ack.Classes;
         Parent_Class : constant Constant_Class_Entity :=
                          Local_Classes.Element (Parent_Name);
         Child_Class  : constant Constant_Class_Entity :=
                          Local_Classes.Element (Child_Name);

         procedure Add_Call
           (Ancestor_Class : not null access constant
              Class_Entity_Record'Class;
            Call_Name      : String);

         --------------
         -- Add_Call --
         --------------

         procedure Add_Call
           (Ancestor_Class : not null access constant
              Class_Entity_Record'Class;
            Call_Name      : String)
         is
            pragma Unreferenced (Ancestor_Class);
         begin

            Calls.Append (Implicit_Call_Record'
                          (Feature_Name => +Call_Name));

--              if Report_Implicit_Calls then
--                 Ada.Text_IO.Put_Line
--                   (Action_File,
--                    "   IO.Put_Line ("""
--                    & Call_Name
--                    & ": "
--                    & Parent_Class.Standard_Name
--                    & "/"
--                    & Child_Class.Standard_Name
--                    & """)");
--              end if;

--              Ada.Text_IO.Put_Line
--                (Action_File,
--                 "   call "
--                 & "tree." & Parent_Class.Link_Name & "."
--                 & Ancestor_Class.Link_Name
--                 & "."
--                 & Call_Name
--                 & "(tree." & Parent_Class.Link_Name & ","
--                 & "child." & Child_Class.Link_Name & ");");

         end Add_Call;

      begin
         Parent_Class.Scan_Conforming_Child_Ancestors
           (Child_Class, Add_Call'Access);
      end Check_Conforming_Children;

      ------------------------
      -- Is_Group_Reference --
      ------------------------

      function Is_Group_Reference
        (Class    : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Property : not null access constant
           Root_Entity_Type'Class)
         return Boolean
      is
         use Ada.Strings.Fixed;
         Property_Type : constant Entity_Type := Property.Get_Type;
         Property_Link_Name : constant String := Property_Type.Link_Name;
         Class_Link_Name    : constant String := Class.Link_Name;
         Property_Top_Name  : constant String :=
                                Property_Link_Name
                                  (Property_Link_Name'First
                                   .. Index (Property_Link_Name, "."));
         Class_Top_Name     : constant String :=
                                Class_Link_Name
                                  (Class_Link_Name'First
                                   .. Index (Class_Link_Name, "."));
         Count              : Natural := 0;
      begin
         for I in Property_Link_Name'First .. Property_Link_Name'Last - 1 loop
            if Property_Link_Name (I .. I) = "." then
               Count := Count + 1;
            end if;
         end loop;

         return Count = 2
           and then Property_Link_Name /= Class_Link_Name
           and then Property_Top_Name = Class_Top_Name;
      end Is_Group_Reference;

      ----------------
      -- Load_Class --
      ----------------

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         Source_Path : constant String :=
                         Ada.Directories.Full_Name (Directory_Entry);
         Source_Name : constant String :=
                         Ada.Directories.Base_Name
                           (Source_Path);
         Match_Name  : constant String :=
                         Grammar.Name
                         & "-"
                         & Aquarius.Actions.Action_Group_Name (Group)
                       & "-";
         Tree_Name   : constant String :=
                         (if Source_Name'Length > Match_Name'Length
                          and then Source_Name (1 .. Match_Name'Length)
                          = Match_Name
                          then Source_Name (Match_Name'Length + 1
                            .. Source_Name'Last)
                          else "");
      begin
         if Report_Class_Load then
            Ada.Text_IO.Put_Line ("loading: " & Source_Name);
         end if;

         References.Clear;
         if Tree_Name /= ""
           and then Grammar.Have_Syntax (Tree_Name)
         then
            Ack.Bindings.Actions.Add_Tree (Binding_Table, Tree_Name);
         end if;

         declare
            use type Ada.Calendar.Time;
            Result : Ack.Compile.Compilation_Result;
         begin
            Ack.Compile.Compile_Class
              (Source_Path, Result,
               Add_Feature_Binding'Access);

            if First_Class
              or else Result.Newest_Class_Source > Newest_Class_Time
            then
               Newest_Class_Time := Result.Newest_Class_Source;
               First_Class := False;
            end if;
         end;

      end Load_Class;

      ----------------------
      -- Load_Object_File --
      ----------------------

      procedure Load_Object_File
        (Feature_Name : String;
         Binding      : Binding_Record)
      is null;
      --     pragma Unreferenced (Feature_Name);
      --     Parent_Name       : constant String := -Binding.Parent_Full_Name;
      --     Parent_Class_Name : constant String :=
      --                       (if Aqua_Bound_Classes.Contains (Parent_Name)
      --                            then Parent_Name & "_Aqua_Binding"
      --                            else Parent_Name);
      --     Object_Name       : constant String :=
      --                 To_Source_File_Name (Parent_Class_Name) & ".o";
      --  begin
      --     Image.Load (Object_Name);
      --  end Load_Object_File;

      -------------------------
      -- To_Source_File_Name --
      -------------------------

      function To_Source_File_Name
        (Class_Name : String)
         return String
      is
         Result : String := Class_Name;
      begin
         for Ch of Result loop
            if Ch = '.' then
               Ch := '-';
            else
               Ch := Ada.Characters.Handling.To_Lower (Ch);
            end if;
         end loop;
         return Result;
      end To_Source_File_Name;

      ------------------------------
      -- Write_Aqua_Binding_Class --
      ------------------------------

      procedure Write_Aqua_Binding_Class is
         use Ada.Strings.Unbounded;
         use Ada.Text_IO;
         File : File_Type;

         procedure Put_Converter
           (Local_Name, Class_Name : String);

         procedure Check_Property
           (Tree_Name, Local_Name, Converter_Name, Class_Name : String;
            Inherits_Program_Tree                             : Boolean);

         procedure Check_Class_Binding
           (Bound_Class_Feature : Constant_Entity_Type);

         procedure Write_Binding
           (Feature_Name : String;
            Binding      : Binding_Record);

         -------------------------
         -- Check_Class_Binding --
         -------------------------

         procedure Check_Class_Binding
           (Bound_Class_Feature : Constant_Entity_Type)
         is
            Bound_Class_Name : constant String :=
                                 Bound_Class_Feature.Get_Type
                                   .Class_Context.Qualified_Name;
         begin
            Put_Line
              (File,
               "      if Parent.Has_Property ("""
               & Bound_Class_Name
               & """) then");
            Put_Line
              (File,
               "         P.Set_Binding_" & Bound_Class_Feature.Declared_Name
               & " (Convert_Ref_" & Bound_Class_Feature.Declared_Name
               & ".To_Object (Parent.Get_Property ("""
               & Bound_Class_Name & """)))");
            Put_Line
              (File,
               "       end");
         end Check_Class_Binding;

         --------------------
         -- Check_Property --
         --------------------

         procedure Check_Property
           (Tree_Name, Local_Name, Converter_Name, Class_Name : String;
            Inherits_Program_Tree                             : Boolean)
         is
         begin
            Put_Line
              (File,
               "      if " & Tree_Name & ".Has_Property ("""
               & Class_Name
               & """) then");
            Put_Line
              (File,
               "         "
               & Local_Name & " := " & Converter_Name
               & ".To_Object (" & Tree_Name
               & ".Get_Property (""" & Class_Name & """))");
            Put_Line
              (File,
               "      else");
            Put_Line
              (File,
               "         "
               & "create " & Local_Name);

            if Inherits_Program_Tree then
               Put_Line
                 (File,
                  "         "
                  & Local_Name & ".Set_Tree ("
                  & Tree_Name & ")");
            end if;

            Put_Line
              (File,
               "         "
               & Tree_Name & ".Set_Property ("""
               & Class_Name & """, " & Converter_Name
               & ".To_Address (" & Local_Name & "))");
            Put_Line
              (File,
               "      end");
         end Check_Property;

         -------------------
         -- Put_Converter --
         -------------------

         procedure Put_Converter
           (Local_Name, Class_Name : String)
         is
         begin
            Put_Line
              (File,
               "      " & Local_Name & " : "
               & "System.Address_To_Object_Conversions["
               & Class_Name & "]");
         end Put_Converter;

         -------------------
         -- Write_Binding --
         -------------------

         procedure Write_Binding
           (Feature_Name : String;
            Binding      : Binding_Record)
         is
            Parent_Name       : constant String := -Binding.Parent_Full_Name;
            Child_Name        : constant String := -Binding.Child_Full_Name;
            Parent_Class_Name : constant String :=
                                  (if Aqua_Bound_Classes.Contains (Parent_Name)
                                   then Parent_Name & "_Aqua_Binding"
                                   else Parent_Name);
            Child_Class_Name  : constant String :=
                                  (if Aqua_Bound_Classes.Contains (Child_Name)
                                   then Child_Name & "_Aqua_Binding"
                                   else Child_Name);
            Parent_Is_Tree    : constant Boolean :=
                                  Binding.Parent_Class.Conforms_To
                                    (Aquarius_Trees_Program_Tree);
            Child_Is_Tree     : constant Boolean :=
                                  (if Child_Name = ""
                                   then False
                                   else Binding.Child_Class.Conforms_To
                                     (Aquarius_Trees_Program_Tree));
            Position          : constant String :=
                                  Position_Name (Binding.Position);
            Child_Tree        : constant String := -Binding.Child_Tree;
            Has_Child         : constant Boolean :=
                                  Child_Name /= "";
            Child_String      : constant Boolean :=
                                  Child_Name = "String";

         begin

            if not Binding.References.Is_Empty then
               declare
                  use Ada.Directories;
                  Parent_Class_File : File_Type;
               begin
                  Aqua_Bound_Classes.Include (Parent_Name);
                  Create (Parent_Class_File, Out_File,
                          Compose
                            (Containing_Directory (Binding_File_Path),
                             To_Source_File_Name (Parent_Class_Name)
                             & ".aqua"));
                  Put_Line (Parent_Class_File,
                            "class " & Parent_Class_Name
                            & " inherit " & Parent_Name);
                  Put_Line (Parent_Class_File,
                            "feature");
                  for Ref of Binding.References loop
                     Put_Line (Parent_Class_File,
                               "   Set_Binding_" & Ref.Declared_Name
                               & " (Item : "
                               & Ref.Get_Type.Class_Context.Qualified_Name
                               & ")");
                     Put_Line (Parent_Class_File,
                               "      do");
                     Put_Line (Parent_Class_File,
                               "         " & Ref.Declared_Name
                               & " := Item");
                     Put_Line (Parent_Class_File,
                               "      end");
                  end loop;

                  Put_Line (Parent_Class_File, "end");
                  Close (Parent_Class_File);
               end;
            end if;

            New_Line (File);

            Put_Line
              (File,
               "   " & Feature_Name
               & " (Top, Parent, Child : Aquarius.Trees.Program_Tree_Driver)");
            Put_Line (File, "   local");
            Put_Converter ("Convert_P", Parent_Class_Name);
            if Has_Child and then not Child_String then
               Put_Converter ("Convert_C", Child_Class_Name);
            end if;
            for Ref of Binding.References loop
               Put_Converter ("Convert_Ref_" & Ref.Declared_Name,
                              Class_Name =>
                                Ref.Get_Type.Class_Context.Qualified_Name);
            end loop;

            Put_Line
              (File,
               "      P : " & Parent_Class_Name);
            if Has_Child then
               Put_Line
                 (File,
                  "      C : " & Child_Class_Name);
            end if;

            Put_Line
              (File,
               "   do");

            if Report_Calls then
               Put_Line
                 (File,
                  "      if attached IO then else create IO end");
            end if;

            Check_Property ("Parent", "P", "Convert_P", Parent_Name,
                            Parent_Is_Tree);

            if Has_Child then
               if Child_String then
                  Put_Line
                    (File,
                     "      C := Child.Text");
               else
                  Check_Property ("Child", "C", "Convert_C", Child_Name,
                                  Child_Is_Tree);
               end if;
            end if;

            for Ref of Binding.References loop
               Check_Class_Binding (Ref);
            end loop;

            if Has_Child then

               for Call of Binding.Implicit_Calls loop
                  Put_Line
                    (File,
                     "      P." & (-Call.Feature_Name)
                         & " (C)");
               end loop;

               if Report_Calls then
                  Put_Line
                    (File,
                     "      IO.Put_Line ("""
                     & Position & " "
                     & Parent_Name & "/" & Child_Name
                     & """)");
               end if;

               if Binding.Has_Feature_Binding then
                  Put_Line
                    (File,
                     "      P." & Position & "_"
                     & Child_Tree & " (C)");
               end if;

            elsif Binding.Has_Feature_Binding then
               if Report_Calls then
                  Put_Line
                    (File,
                     "      IO.Put_Line ("""
                     & Position & " "
                     & Parent_Name
                     & """)");
               end if;

               Put_Line
                 (File,
                  "      P." & Position & "_Node");
            end if;

            Put_Line
              (File,
               "      Exit (0)");
            Put_Line (File, "   end");
         end Write_Binding;

      begin
         Create (File, Out_File, Binding_File_Path);
         Put_Line (File, "note");

         for Index in 1 .. Binding_Vector.Last_Index loop
            declare
               Rec           : constant Binding_Record :=
                                 Binding_Vector.Element (Index);
               Feature_Name  : constant String := Binding_Name (Index);
               Parent_Name   : constant String :=
                                 To_String (Rec.Parent_Tree);
               Child_Name    : constant String :=
                                 To_String (Rec.Child_Tree);
            begin
               if Rec.Has_Feature_Binding
                 or else not Rec.Implicit_Calls.Is_Empty
               then
                  Put_Line
                    (File,
                     "   Aqua_Action_Binding_" & Feature_Name
                     & ": "
                     & Parent_Name
                     & ", "
                     & Position_Name (Rec.Position)
                     & (if Child_Name = "" then ""
                       else ", " & Child_Name));
               end if;
            end;
         end loop;

         New_Line (File);
         Put_Line (File, "expanded class");
         Put_Line (File, "   " & Grammar.Name & "."
                   & Group_Name
                   & ".Action_Bindings");
         New_Line (File);
         Put_Line (File, "feature");
         New_Line (File);
         Put_Line (File, "   Exit (Code : Integer) external ""intrinsic""");

         if Report_Calls then
            Put_Line (File, "   IO : Aqua.Text_IO");
         end if;

         for Index in 1 .. Binding_Vector.Last_Index loop
            declare
               Rec           : constant Binding_Record :=
                                 Binding_Vector.Element (Index);
            begin

               if Rec.Has_Feature_Binding
                 or else not Rec.Implicit_Calls.Is_Empty
               then
                  Write_Binding (Binding_Name (Index), Rec);
               end if;

            end;
         end loop;

         New_Line (File);
         Put_Line (File, "end");
         Close (File);
      end Write_Aqua_Binding_Class;

   begin

      Ada.Directories.Search
        (Base_Aqua_Path,
         Grammar.Name & "-" & Group_Name & "*.aqua",
         Process => Load_Class'Access);

      Ack.Bindings.Actions.Scan_Trees
        (Binding_Table, Check_Bindings'Access);

      if not Ack.Errors.Has_Errors then

         for Index in 1 .. Binding_Vector.Last_Index loop
            declare
               Rec           : constant Binding_Record :=
                                 Binding_Vector.Element (Index);
               Parent_Name   : constant String :=
                                 -Rec.Parent_Full_Name;
            begin
               if Rec.Has_Feature_Binding
                 or else not Rec.Implicit_Calls.Is_Empty
               then
                  if not Rec.References.Is_Empty then
                     Aqua_Bound_Classes.Include (Parent_Name);
                  end if;
               end if;
            end;
         end loop;

         declare
            use Ada.Calendar, Ada.Directories;
            Object_Path : constant String :=
                            Aquarius.Configuration.Object_File_Path
                              (Base_Name (Binding_File_Path));
         begin

            if First_Class
              or else not Exists (Object_Path)
              or else
                (not Exists (Binding_File_Path)
                 or else Modification_Time (Binding_File_Path)
                 < Newest_Class_Time)
            then
               Write_Aqua_Binding_Class;

               declare
                  Result : Ack.Compile.Compilation_Result;
               begin
                  Ack.Compile.Compile_Class
                    (Binding_File_Path, Result, null);
               end;

            else

               declare
                  Result : Ack.Compile.Compilation_Result;
               begin
                  Ack.Compile.Compile_Class
                    (Binding_File_Path, Result, null);
               end;

               for Index in 1 .. Binding_Vector.Last_Index loop
                  declare
                     Rec  : constant Binding_Record :=
                              Binding_Vector.Element (Index);
                     Name : constant String := Binding_Name (Index);
                  begin

                     if (Rec.Has_Feature_Binding
                         or else not Rec.Implicit_Calls.Is_Empty)
                       and then not Rec.References.Is_Empty
                     then
                        Load_Object_File (Name, Rec);
                     end if;

                  end;
               end loop;
               --  Image.Load (Base_Name (Binding_File_Path) & ".o32");
               --  Image.Load ("system-address_to_object_conversions.o32");
            end if;
         end;
      end if;

      return not Ack.Errors.Has_Errors;

   end Load_Ack_Binding;

end Ack.Bindings;
