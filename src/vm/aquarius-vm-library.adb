with Ada.Characters.Handling;
with Ada.Tags;

with Aquarius.Grammars;
with Aquarius.Programs;
with Aquarius.Programs.Parser;
with Aquarius.Properties.String_Sets;
with Aquarius.Trees;

with Aquarius.VM.Maps;

package body Aquarius.VM.Library is

   Have_Standard_Library  : Boolean        := False;
   Local_Standard_Library : VM_Environment;

   procedure Derive
     (Env                : VM_Environment;
      Base_Class_Name    : String;
      Derived_Class_Name : String;
      Derived_Class      : in out VM_Value_Record);

   function Create_Standard_Library
     return VM_Environment;

   function Eval_Get_Named_Direct_Child
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Create_Map
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Create_Tree
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Create_Stub
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Parse
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Build_Tree
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Ada_Name
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_First
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Image
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Include
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Set_Contains
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Last
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Length
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Name
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Map_Insert
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Map_Replace
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Map_Contains
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_Map_Element
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Eval_Method_String_Value
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   -----------------------------
   -- Create_Standard_Library --
   -----------------------------

   function Create_Standard_Library
     return VM_Environment
   is
      Env : constant VM_Environment :=
              New_Environment ("standard", Null_Environment);
      No_Default_Values : Array_Of_Values (1 .. 0);
   begin
      Insert (Env, "->",
              To_Value (Eval_Get_Named_Direct_Child'Access,
                (Null_Value, Null_Value,
                 To_Value (1))));
      Insert (Env, "create_map",
              To_Value
                (Eval_Create_Map'Access,
                 No_Default_Values));
      Insert (Env, "create_tree",
              To_Value (Eval_Create_Tree'Access,
                (1 => Null_Value)));
      Insert (Env, "create_stub",
              To_Value (Eval_Create_Stub'Access,
                (1 => Null_Value,
                 2 => Null_Value,
                 3 => Null_Value,
                 4 => Null_Value)));
      Insert (Env, "parse",
              To_Value (Eval_Parse'Access,
                (1 => Null_Value,
                 2 => Null_Value)));
      Insert (Env, "build_tree",
              To_Value (Eval_Build_Tree'Access,
                (1 => Null_Value)));

      declare
         use Ada.Strings.Unbounded;
         Class_Name   : constant String :=
                          Ada.Tags.External_Tag
                            (Root_Aquarius_Object'Tag);
         Object_Class : VM_Value_Record :=
                          (Val_Class,
                           To_Unbounded_String (Class_Name),
                           String_Vectors.Empty_Vector);
      begin
         Object_Class.Member_Names.Append ("name");
         Insert (Env, Class_Name, New_Value (Object_Class));
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "name",
            Value       => To_Value (Eval_Method_Name'Access, 1));
      end;

      declare
         use Ada.Strings.Unbounded;
         Class_Name : constant String :=
                        Ada.Tags.External_Tag
                          (Aquarius.Trees.Root_Tree_Type'Tag);
         Tree_Class : VM_Value_Record :=
                        (Val_Class,
                         To_Unbounded_String (Class_Name),
                         String_Vectors.Empty_Vector);
      begin
         Derive (Env, Ada.Tags.External_Tag (Root_Aquarius_Object'Tag),
                 Class_Name, Tree_Class);
         Tree_Class.Member_Names.Append ("first");
         Tree_Class.Member_Names.Append ("last");
         Tree_Class.Member_Names.Append ("length");
         Insert (Env, Class_Name, New_Value (Tree_Class));
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "first",
            Value       => To_Value (Eval_Method_First'Access, 1));
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "last",
            Value       => To_Value (Eval_Method_Last'Access, 1));
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "length",
            Value       => To_Value (Eval_Method_Length'Access, 1));
      end;

      declare
         use Ada.Strings.Unbounded;
         Class_Name         : constant String :=
                                Ada.Tags.External_Tag
                                  (Aquarius.Programs.Program_Tree_Type'Tag);
         Program_Tree_Class : VM_Value_Record :=
                                (Val_Class,
                                 To_Unbounded_String (Class_Name),
                                 String_Vectors.Empty_Vector);
      begin
         Derive (Env,
                 Ada.Tags.External_Tag (Aquarius.Trees.Root_Tree_Type'Tag),
                 Class_Name, Program_Tree_Class);
         Program_Tree_Class.Member_Names.Append ("image");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "image",
            Value       => To_Value (Eval_Method_Image'Access, 1));
         Program_Tree_Class.Member_Names.Append ("string_value");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "string_value",
            Value       => To_Value (Eval_Method_String_Value'Access, 1));
         if False then
            Program_Tree_Class.Member_Names.Append ("ada_name");
            Insert
              (Env         => Env,
               Class_Name  => Class_Name,
               Method_Name => "ada_name",
               Value       => To_Value (Eval_Method_Ada_Name'Access, 1));
         end if;
      end;

      declare
         use Ada.Strings.Unbounded;
         use Aquarius.Properties.String_Sets;
         Class_Name         : constant String :=
                                Ada.Tags.External_Tag
                                  (String_Set_Property_Type'Tag);
         String_Set_Class   : VM_Value_Record :=
                                (Val_Class,
                                 To_Unbounded_String (Class_Name),
                                 String_Vectors.Empty_Vector);
      begin
         Derive (Env, Ada.Tags.External_Tag (Root_Aquarius_Object'Tag),
                 Class_Name, String_Set_Class);
         String_Set_Class.Member_Names.Append ("include");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "include",
            Value       => To_Value (Eval_Method_Include'Access, 2));
         String_Set_Class.Member_Names.Append ("contains");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "contains",
            Value       => To_Value (Eval_Method_Set_Contains'Access, 2));
      end;

      declare
         use Ada.Strings.Unbounded;
         use Aquarius.VM.Maps;
         Class_Name         : constant String :=
                                Ada.Tags.External_Tag
                                  (Map_Property_Type'Tag);
         Map_Class   : VM_Value_Record :=
                                (Val_Class,
                                 To_Unbounded_String (Class_Name),
                                 String_Vectors.Empty_Vector);
      begin
         Derive (Env, Ada.Tags.External_Tag (Root_Aquarius_Object'Tag),
                 Class_Name, Map_Class);
         Map_Class.Member_Names.Append ("insert");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "insert",
            Value       => To_Value (Eval_Method_Map_Insert'Access, 3));
         Map_Class.Member_Names.Append ("replace");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "replace",
            Value       => To_Value (Eval_Method_Map_Replace'Access, 3));
         Map_Class.Member_Names.Append ("contains");
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "contains",
            Value       => To_Value (Eval_Method_Map_Contains'Access, 2));
         Insert
           (Env         => Env,
            Class_Name  => Class_Name,
            Method_Name => "element",
            Value       => To_Value (Eval_Method_Map_Element'Access, 2));
      end;

      return Env;
   end Create_Standard_Library;

   ------------
   -- Derive --
   ------------

   procedure Derive
     (Env                : VM_Environment;
      Base_Class_Name    : String;
      Derived_Class_Name : String;
      Derived_Class      : in out VM_Value_Record)
   is
      Base_Class : constant VM_Value :=
                     Get_Value (Env, Base_Class_Name);
   begin
      for M of Base_Class.Member_Names loop
         Insert (Env, Derived_Class_Name, M,
                 Get_Value (Env, Base_Class_Name & "__" & M));
         Derived_Class.Member_Names.Append (M);
      end loop;
   end Derive;

   ----------------------
   -- Eval_Build_Tree --
   ----------------------

   function Eval_Build_Tree
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      use Aquarius.Programs;
      Grammar        : constant Aquarius.Grammars.Aquarius_Grammar :=
                         Aquarius.Grammars.Aquarius_Grammar
                           (Aquarius.VM.To_Property
                              (Aquarius.VM.Get_Value (Env, "grammar")));
      Tree_Name_List : constant VM_Value := Args (Args'First);
      Top_Tree_Name  : constant String   :=
                         To_String (Head (Tree_Name_List));
      Top_Tree       : constant Program_Tree :=
                         Grammar.Make_Program_Tree (Top_Tree_Name);
      It_Tree        : Program_Tree := Top_Tree;
      It_List        : VM_Value := Tail (Tree_Name_List);
   begin
      while It_List /= Null_Value loop
         declare
            Child_Name : constant String :=
                           To_String (Head (It_List));
            Child      : constant Program_Tree :=
                           Grammar.Make_Program_Tree (Child_Name);
         begin
            Aquarius.Programs.Parser.Parse_Tree (It_Tree, Child);
            It_Tree := Child;
            It_List := Tail (It_List);
         end;
      end loop;

      return To_Value (Top_Tree);

   end Eval_Build_Tree;

   ---------------------
   -- Eval_Create_Map --
   ---------------------

   function Eval_Create_Map
     (Env       : VM_Environment;
      Arguments : Array_Of_Values)
      return VM_Value
   is
      pragma Unreferenced (Env);
      pragma Unreferenced (Arguments);
      New_Map : constant Maps.Map_Property_Access :=
                  new Maps.Map_Property_Type;
   begin
      return VM.To_Value (New_Map);
   end Eval_Create_Map;

   ----------------------
   -- Eval_Create_Stub --
   ----------------------

   function Eval_Create_Stub
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      Grammar     : constant Aquarius.Grammars.Aquarius_Grammar :=
                      Aquarius.Grammars.Aquarius_Grammar
                        (Aquarius.VM.To_Property
                           (Aquarius.VM.Get_Value (Env, "grammar")));
      Top_Name    : constant String :=
                      Aquarius.VM.To_String (Args (Args'First));
      Before_Text : constant String :=
                      Aquarius.VM.To_String (Args (Args'First + 1));
      Middle_Name : constant String :=
                      Aquarius.VM.To_String (Args (Args'First + 2));
      After_Text  : constant String :=
                      Aquarius.VM.To_String (Args (Args'First + 3));
      Top_Tree    : constant Aquarius.Programs.Program_Tree :=
                      Grammar.Make_Program_Tree (Top_Name);
      Middle_Tree : constant Aquarius.Programs.Program_Tree :=
                      Grammar.Make_Program_Tree (Middle_Name);
   begin
      Aquarius.Programs.Parser.Parse_Tree
        (Top    => Top_Tree,
         Before => Before_Text,
         Child  => Middle_Tree,
         After  => After_Text);
      return To_Value (Top_Tree);
   end Eval_Create_Stub;

   ----------------------
   -- Eval_Create_Tree --
   ----------------------

   function Eval_Create_Tree
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      Tree_Name : constant String :=
                    Aquarius.VM.To_String (Args (Args'First));
      Grammar   : constant Aquarius.Grammars.Aquarius_Grammar :=
                    Aquarius.Grammars.Aquarius_Grammar
                      (Aquarius.VM.To_Property
                         (Aquarius.VM.Get_Value (Env, "grammar")));
      Result    : constant Aquarius.Programs.Program_Tree :=
                    Grammar.Make_Program_Tree (Tree_Name);
   begin
      Result.Expand;
      return To_Value (Result);
   end Eval_Create_Tree;

   ---------------------------------
   -- Eval_Get_Named_Direct_Child --
   ---------------------------------

   function Eval_Get_Named_Direct_Child
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      T     : constant Aquarius.Trees.Tree :=
                Aquarius.Trees.Tree (To_Property (Args (Args'First)));
      Ts    : constant Aquarius.Trees.Array_Of_Trees :=
                T.Get_Named_Children;
      Name  : constant String := To_String (Args (Args'First + 1));
      Index : Natural := To_Integer (Args (Args'First + 2));
   begin
      for I in Ts'Range loop
         if Ts (I).Name = Name then
            Index := Index - 1;
            if Index = 0 then
               return To_Value (Ts (I));
            end if;
         end if;
      end loop;

      return Error_Value ("no such child: " & Name & Index'Img);

   end Eval_Get_Named_Direct_Child;

   --------------------------
   -- Eval_Method_Ada_Name --
   --------------------------

   function Eval_Method_Ada_Name
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Programs;
      Program : constant Program_Tree :=
                  Program_Tree (Args (Args'First).Prop_Value);
      Result  : String := Program.Concatenate_Children;
      First   : Boolean := True;
   begin
      for I in Result'Range loop
         case Result (I) is
            when '_' | '.' =>
               First := True;
            when 'a' .. 'z' =>
               if First then
                  Result (I) := Ada.Characters.Handling.To_Upper (Result (I));
               end if;
               First := False;
            when others =>
               null;
         end case;
      end loop;
      return To_Value (Result);
   end Eval_Method_Ada_Name;

   -----------------------
   -- Eval_Method_First --
   -----------------------

   function Eval_Method_First
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Trees;
   begin
      return To_Value (Tree (Args (Args'First).Prop_Value).First_Leaf);
   end Eval_Method_First;

   -----------------------
   -- Eval_Method_Image --
   -----------------------

   function Eval_Method_Image
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Programs;
      Program : constant Program_Tree :=
                  Program_Tree (Args (Args'First).Prop_Value);
      Result  : constant String := Program.Concatenate_Children;
   begin
      return To_Value (Result);
   end Eval_Method_Image;

   -------------------------
   -- Eval_Method_Include --
   -------------------------

   function Eval_Method_Include
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Properties.String_Sets;
      Set : constant access String_Set_Property_Type'Class :=
              String_Set_Property_Type'Class
                (Args (Args'First).Prop_Value.all)'Access;
      Value : constant String := To_String (Args (Args'First + 1));
   begin
      Set.Include (Value);
      return Args (Args'First);
   end Eval_Method_Include;

   ----------------------
   -- Eval_Method_Last --
   ----------------------

   function Eval_Method_Last
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Trees;
   begin
      return To_Value (Tree (Args (Args'First).Prop_Value).Last_Leaf);
   end Eval_Method_Last;

   ------------------------
   -- Eval_Method_Length --
   ------------------------

   function Eval_Method_Length
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Trees;
   begin
      return To_Value (Tree (Args (Args'First).Prop_Value).Child_Count);
   end Eval_Method_Length;

   ------------------------------
   -- Eval_Method_Map_Contains --
   ------------------------------

   function Eval_Method_Map_Contains
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.VM.Maps;
      Map : constant access Map_Property_Type'Class :=
              Map_Property_Type'Class
                (Args (Args'First).Prop_Value.all)'Access;
      Key : constant String := To_String (Args (Args'First + 1));
   begin
      return To_Value (Map.Contains (Key));
   end Eval_Method_Map_Contains;

   -----------------------------
   -- Eval_Method_Map_Element --
   -----------------------------

   function Eval_Method_Map_Element
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.VM.Maps;
      Map : constant access Map_Property_Type'Class :=
              Map_Property_Type'Class
                (Args (Args'First).Prop_Value.all)'Access;
      Key : constant String := To_String (Args (Args'First + 1));
   begin
      return Map.Element (Key);
   end Eval_Method_Map_Element;

   ----------------------------
   -- Eval_Method_Map_Insert --
   ----------------------------

   function Eval_Method_Map_Insert
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.VM.Maps;
      Map : constant access Map_Property_Type'Class :=
              Map_Property_Type'Class
                (Args (Args'First).Prop_Value.all)'Access;
      Key : constant String := To_String (Args (Args'First + 1));
      Value : constant VM_Value := Args (Args'First + 2);
   begin
      Map.Insert (Key, Value);
      return Args (Args'First);
   end Eval_Method_Map_Insert;

   -----------------------------
   -- Eval_Method_Map_Replace --
   -----------------------------

   function Eval_Method_Map_Replace
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.VM.Maps;
      Map : constant access Map_Property_Type'Class :=
              Map_Property_Type'Class
                (Args (Args'First).Prop_Value.all)'Access;
      Key : constant String := To_String (Args (Args'First + 1));
      Value : constant VM_Value := Args (Args'First + 2);
   begin
      Map.Replace (Key, Value);
      return Args (Args'First);
   end Eval_Method_Map_Replace;

   ----------------------
   -- Eval_Method_Name --
   ----------------------

   function Eval_Method_Name
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
   begin
      return To_Value (Args (Args'First).Prop_Value.Name);
   end Eval_Method_Name;

   ------------------------------
   -- Eval_Method_Set_Contains --
   ------------------------------

   function Eval_Method_Set_Contains
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Properties.String_Sets;
      Set : constant access String_Set_Property_Type'Class :=
              String_Set_Property_Type'Class
                (Args (Args'First).Prop_Value.all)'Access;
      Value : constant String := To_String (Args (Args'First + 1));
   begin
      return To_Value (Set.Member (Value));
   end Eval_Method_Set_Contains;

   ------------------------------
   -- Eval_Method_String_Value --
   ------------------------------

   function Eval_Method_String_Value
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.Programs;
      Program : constant Program_Tree :=
                      Program_Tree (Args (Args'First).Prop_Value);
      Raw_Text    : constant String := Program.Text;
      String_Text : String (Raw_Text'Range);
      Count       : Natural := String_Text'First - 1;
      Index       : Positive := Raw_Text'First + 1;
   begin
      --  Remove surrounding quotes, and convert two double quotes
      --  into one.
      while Index < Raw_Text'Last loop
         if Index < Raw_Text'Last - 1
           and then Raw_Text (Index) = '"'
           and then Raw_Text (Index + 1) = '"'
         then
            Index := Index + 1;
         end if;
         Count := Count + 1;
         String_Text (Count) := Raw_Text (Index);
         Index := Index + 1;
      end loop;
      return To_Value (String_Text (String_Text'First .. Count));
   end Eval_Method_String_Value;

   ----------------
   -- Eval_Parse --
   ----------------

   function Eval_Parse
     (Env  : Aquarius.VM.VM_Environment;
      Args : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      Top_Tree : constant Aquarius.Programs.Program_Tree :=
                   Aquarius.Programs.Program_Tree
                     (VM.To_Tree (Args (Args'First)));
      Code     : constant String :=
                   Aquarius.VM.To_String (Args (Args'First + 1));
   begin
      Aquarius.Programs.Parser.Parse_Tree
        (Top    => Top_Tree,
         Code   => Code);
      return To_Value (Top_Tree);
   end Eval_Parse;

   ----------------------
   -- Standard_Library --
   ----------------------

   function Standard_Library return VM_Environment is
   begin
      if not Have_Standard_Library then
         Local_Standard_Library := Create_Standard_Library;
         Have_Standard_Library  := True;
      end if;

      return Local_Standard_Library;
   end Standard_Library;

end Aquarius.VM.Library;
