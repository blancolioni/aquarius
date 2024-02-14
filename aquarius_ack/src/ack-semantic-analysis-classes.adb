with Ada.Text_IO;

with Ack.Environment;

with Ack.Generate.Primitives;

with Ack.Semantic.Classes;
with Ack.Semantic.Work;

with Ack.Semantic.Analysis.Class_Names;
with Ack.Semantic.Analysis.Types;

with Ack.Types;

package body Ack.Semantic.Analysis.Classes is

   -------------------------------
   -- Analyse_Class_Declaration --
   -------------------------------

   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is

      Notes_Node       : constant Node_Id := Notes (Node);
      Inheritance_Node : constant Node_Id := Inheritance (Node);
      Features_Node    : constant Node_Id := Class_Features (Node);
      Class            : constant Ack.Classes.Class_Entity :=
                           Analyse_Class_Header (Node, Class_Header (Node));

      procedure Add_Feature_Work_Items
        (Features_Node : Node_Id;
         Category      : Ack.Semantic.Work.Work_Item_Category);

      ----------------------------
      -- Add_Feature_Work_Items --
      ----------------------------

      procedure Add_Feature_Work_Items
        (Features_Node : Node_Id;
         Category      : Ack.Semantic.Work.Work_Item_Category)
      is
         Clause_List : constant List_Id :=
                         Feature_Clauses (Features_Node);
      begin
         for Clause_Node of List_Table.Element (Clause_List).List loop
            declare
               Feature_List : constant List_Id :=
                                Feature_Declarations (Clause_Node);
            begin
               for Feature_Node of List_Table.Element (Feature_List).List loop
                  Ack.Semantic.Work.Add_Work_Item
                    (Category => Category,
                     Class    => Class,
                     Feature  => Feature_Node);
               end loop;
            end;
         end loop;
      end Add_Feature_Work_Items;

   begin

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing: " & Class.Qualified_Name);
      end if;

      if Notes_Node in Real_Node_Id then
         Analyse_Notes (Class, Notes_Node);
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Analysing feature names: " & Class.Qualified_Name);
         end if;

         Analyse_Features (Class, Features_Node,
                           Analyse_Feature_Name'Access);
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing inheritance: " & Class.Qualified_Name);
      end if;

      if Inheritance_Node /= No_Node then
         Analyse_Inheritance (Class, Inheritance_Node);
      elsif Class.Standard_Name /= "any" then
         Class.Inherit
           (Ack.Types.New_Class_Type
              (Node       => Node,
               Class      =>
                 Ack.Semantic.Classes.Load_Class
                   (Get_Program (Node), Ack.Environment.Top_Level,
                    Get_Name_Id ("Any")),
               Detachable => False));
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Adding feature work items: " & Class.Qualified_Name);
         end if;

         Add_Feature_Work_Items
           (Features_Node, Ack.Semantic.Work.Feature_Header);

      end if;

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Class_Binding,
         Class     => Class,
         Feature   => No_Node);

      if Features_Node in Real_Node_Id then

         Add_Feature_Work_Items
           (Features_Node, Ack.Semantic.Work.Feature_Body);

      end if;

      if not Class.Deferred then
         declare
            procedure Check_Effective
              (Feature : not null access
                 Ack.Features.Feature_Entity_Record'Class);

            ---------------------
            -- Check_Effective --
            ---------------------

            procedure Check_Effective
              (Feature : not null access
                 Ack.Features.Feature_Entity_Record'Class)
            is
            begin
               if Feature.Deferred then
                  Error
                    (Node   => Class.Declaration_Node,
                     Kind   => E_Requires_Definition,
                     Entity => Constant_Entity_Type (Feature));
               end if;
            end Check_Effective;

         begin
            Class.Scan_Features (Check_Effective'Access);
         end;
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("  virtual and object tables: " & Class.Qualified_Name);
      end if;

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Class_Layout,
         Class     => Class,
         Feature   => No_Node);

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Error_Report,
         Class     => Class,
         Feature   => No_Node);

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Finished: " & Class.Qualified_Name);
      end if;

   end Analyse_Class_Declaration;

   --------------------------
   -- Analyse_Class_Header --
   --------------------------

   function Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id)
      return Ack.Classes.Class_Entity
   is
      Formal_Generics_Node : constant Node_Id := Formal_Generics (Header);
      Creators_Node        : constant Node_Id := Class_Creators (Class);
      Result               : Ack.Classes.Class_Entity;
   begin
      Class_Names.Analyse_Class_Name
        (null, Class_Name (Header),
         Defining_Name => True);
      Result := Ack.Classes.Get_Class_Entity (Class_Name (Header));

      Result.Set_Top_Class_Node (Class);

      if Node_Table.Element (Header).Deferred then
         Result.Set_Deferred;
      elsif Node_Table.Element (Header).Expanded then
         Result.Set_Expanded;
      elsif Node_Table.Element (Header).Frozen then
         Result.Set_Frozen;
      end if;

      Set_Entity (Class, Result);
      if Formal_Generics_Node /= No_Node then
         Analyse_Formal_Generics (Result, Formal_Generics_Node);
      end if;

      if Creators_Node in Real_Node_Id then
         declare
            procedure Add_Creator_Clause
              (Node : Node_Id);

            ------------------------
            -- Add_Creator_Clause --
            ------------------------

            procedure Add_Creator_Clause
              (Node : Node_Id)
            is
               procedure Add_Creator_Name (Creator_Node : Node_Id);

               ----------------------
               -- Add_Creator_Name --
               ----------------------

               procedure Add_Creator_Name (Creator_Node : Node_Id) is
               begin
                  Result.Add_Creator
                    (Get_Name (Creator_Node));
               end Add_Creator_Name;

            begin
               Scan (Creator_List (Node), Add_Creator_Name'Access);
            end Add_Creator_Clause;

         begin
            Scan (Creator_Clauses (Creators_Node),
                  Add_Creator_Clause'Access);
         end;
      end if;

      return Result;
   end Analyse_Class_Header;

   --------------------------
   -- Analyse_Feature_Name --
   --------------------------

   procedure Analyse_Feature_Name
     (Class    : Ack.Classes.Class_Entity;
      Exports  : Node_Id;
      Feature  : Node_Id)
   is
      pragma Unreferenced (Exports);
      Names        : constant List_Id := New_Feature_List (Feature);
      Dec_Body        : constant Node_Id := Declaration_Body (Feature);
      Value_Node      : constant Node_Id := Value (Dec_Body);
      Routine_Feature : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Routine;
      Deferred        : constant Boolean :=
                          Routine_Feature
                              and then Node_Table.Element
                                (Value_Node).Deferred;
   begin
      for Node of List_Table.Element (Names).List loop
         declare
            Extended_Name_Node : constant Node_Id :=
                                   Extended_Feature_Name (Node);
            Name_Node          : constant Node_Id :=
                                   Feature_Name (Extended_Name_Node);
            Alias_Node         : constant Node_Id :=
                                   Feature_Alias (Extended_Name_Node);
            Alias              : constant Name_Id :=
                                   (if Alias_Node = No_Node then No_Name
                                    else Get_Name (Alias_Node));
            Entity             : constant Ack.Features.Feature_Entity :=
                                   Ack.Features.New_Feature
                                     (Name        => Get_Name (Name_Node),
                                      Alias       => Alias,
                                      Declaration => Node,
                                      Property    =>
                                        Property_Feature_Node (Feature),
                                      Class       => Class);
         begin
            if Entity.Standard_Name = "void"
              and then Class.Standard_Name /= "any"
            then
               Error (Name_Node, E_Illegal_Redefinition);
            end if;

            Ack.Features.Set_Feature_Entity (Node, Entity);
            if Deferred then
               Entity.Set_Deferred;
            end if;
            Class.Add_Feature (Entity);
         end;
      end loop;
   end Analyse_Feature_Name;

   ----------------------
   -- Analyse_Features --
   ----------------------

   procedure Analyse_Features
     (Class   : Ack.Classes.Class_Entity;
      Node    : Node_Id;
      Analyse : not null access
        procedure (Class : Ack.Classes.Class_Entity;
                   Exports  : Node_Id;
                   Node  : Node_Id))
   is
      Clause_List : constant List_Id :=
                      Feature_Clauses (Node);
   begin
      for Clause_Node of List_Table.Element (Clause_List).List loop
         declare
            Feature_List : constant List_Id :=
                             Feature_Declarations (Clause_Node);
         begin
            for Feature_Node of List_Table.Element (Feature_List).List loop
               Analyse (Class    => Class,
                        Exports  => No_Node,
                        Node     => Feature_Node);
            end loop;
         end;
      end loop;
   end Analyse_Features;

   -----------------------------
   -- Analyse_Formal_Generics --
   -----------------------------

   procedure Analyse_Formal_Generics
     (Class           : Ack.Classes.Class_Entity;
      Formal_Generics : Node_Id)
   is
      procedure Analyse_Formal_Generic (Node : Node_Id);

      ----------------------------
      -- Analyse_Formal_Generic --
      ----------------------------

      procedure Analyse_Formal_Generic (Node : Node_Id) is
         Name : constant Name_Id :=
                  Get_Name (Formal_Generic_Name (Node));
         Generic_Entity : constant Ack.Types.Type_Entity :=
                            Ack.Types.New_Generic_Formal_Type
                              (Name, Node, Class);
      begin
         Set_Entity (Node, Generic_Entity);
         Class.Add_Generic_Formal (Generic_Entity);
      end Analyse_Formal_Generic;

   begin
      Scan (Formal_Generics_List (Formal_Generics),
            Analyse_Formal_Generic'Access);
   end Analyse_Formal_Generics;

   ---------------------
   -- Analyse_Inherit --
   ---------------------

   procedure Analyse_Inherit
     (Class   : Ack.Classes.Class_Entity;
      Inherit : Node_Id)
   is
      Class_Type      : constant Node_Id := Inherit_Class_Type (Inherit);
      Redefine_List   : constant List_Id := Redefine (Inherit);

      Inherited_Type  : Ack.Types.Type_Entity;
      Inherited_Class : Ack.Classes.Class_Entity;

--        Redefined_Features : WL.String_Sets.Set;

      procedure Set_Redefine (Node : Node_Id);

--        procedure Check_Redefined
--          (Feature : not null access constant
--             Ack.Features.Feature_Entity_Record'Class);
--
--        ---------------------
--        -- Check_Redefined --
--        ---------------------
--
--        procedure Check_Redefined
--          (Feature : not null access constant
--             Ack.Features.Feature_Entity_Record'Class)
--        is
--        begin
--           if Inherited_Class.Feature
--             (Get_Name_Id (Feature.Standard_Name)).Deferred
--             and then not Redefined_Features.Contains (Feature.Standard_Name)
--           then
--              Error (Inherit, E_Missing_Redefine, Feature);
--           end if;
--        end Check_Redefined;

      ------------------
      -- Set_Redefine --
      ------------------

      procedure Set_Redefine (Node : Node_Id) is
         Name : constant Name_Id := Get_Name (Node);
      begin
         Class.Redefine (Node, Inherited_Class, Name);

--           Redefined_Features.Insert (To_Standard_String (Name));
--           if Inherited_Class.Has_Feature (Name) then
--              if Class.Has_Feature (Name) then
--                 Class.Feature (Name).Set_Redefined
--                   (Class            => Class,
--                    Original_Feature =>
--                      Inherited_Class.Feature (Name));
--              else
--                 Error (Node, E_Missing_Redefinition,
--                        Entity_Type (Inherited_Class));
--              end if;
--           else
--              Error (Node, E_Not_Defined_In, Entity_Type (Inherited_Class));
--           end if;
      end Set_Redefine;

   begin

      Ack.Semantic.Analysis.Types.Analyse_Class_Type (Class, Class_Type);

      if Ack.Types.Has_Type_Entity (Class_Type) then
         Inherited_Type := Ack.Types.Get_Type_Entity (Class_Type);
         Inherited_Class := Inherited_Type.Class;

         Set_Entity (Inherit, Inherited_Type);

         Class.Inherit (Inherited_Type);
         Scan (Redefine_List, Set_Redefine'Access);

--           if not Class.Deferred then
--              Inherited_Class.Scan_Features
--                (Check_Redefined'Access);
--           end if;
      end if;

   end Analyse_Inherit;

   -------------------------
   -- Analyse_Inheritance --
   -------------------------

   procedure Analyse_Inheritance
     (Class       : Ack.Classes.Class_Entity;
      Inheritance : Node_Id)
   is
      procedure Analyse (Node : Node_Id);

      -------------
      -- Analyse --
      -------------

      procedure Analyse (Node : Node_Id) is
      begin
         Analyse_Inherit (Class, Node);
      end Analyse;

   begin
      Scan (Inherits (Inheritance), Analyse'Access);
   end Analyse_Inheritance;

   -------------------
   -- Analyse_Notes --
   -------------------

   procedure Analyse_Notes
     (Class : Ack.Classes.Class_Entity;
      Notes : Node_Id)
   is
      List : constant List_Id := Node_Table.Element (Notes).List;

      Have_Single_Item    : Boolean;
      Have_Single_Integer : Boolean := False;
      Single_Integer      : Integer;

      procedure Add_Note_Items
        (Note_Name : String;
         List      : List_Id);

      --------------------
      -- Add_Note_Items --
      --------------------

      procedure Add_Note_Items
        (Note_Name : String;
         List      : List_Id)
      is

         procedure Add_Note_Item
           (Note_Item : Node_Id)
           with Pre => Kind (Note_Item) = N_Note_Item;

         procedure Add_Note_Text
           (Text : String);

         -------------------
         -- Add_Note_Item --
         -------------------

         procedure Add_Note_Item
           (Note_Item : Node_Id)
         is
            Item : constant Node_Id := Field_1 (Note_Item);

         begin
            if Kind (Item) = N_Identifier then
               Add_Note_Text (To_Standard_String (Get_Name (Item)));
            elsif Kind (Item) = N_Constant then
               declare
                  Value : constant Node_Id := Field_2 (Item);
               begin
                  case N_Constant_Value (Kind (Value)) is
                     when N_String_Constant =>
                        Add_Note_Text (To_String (Get_Text (Value)));
                     when N_Character_Constant =>
                        Add_Note_Text (To_String (Get_Name (Value)));
                     when N_Integer_Constant =>
                        Add_Note_Text (To_String (Get_Name (Value)));
                        if Have_Single_Item then
                           Have_Single_Integer := True;
                           Single_Integer :=
                             Integer'Value (To_String (Get_Name (Value)));
                        end if;

                     when N_Boolean_Constant =>
                        Add_Note_Text (if Boolean_Value (Value)
                                       then "True" else "False");
                  end case;
               end;
            else
               raise Constraint_Error with
               Get_Program (Item).Show_Location
                 & ": expected an identifier or a constant, but found "
                 & Node_Kind'Image (Kind (Item));
            end if;
         end Add_Note_Item;

         -------------------
         -- Add_Note_Text --
         -------------------

         procedure Add_Note_Text
           (Text : String)
         is
         begin
            Class.Add_Note (Note_Name, Text);
         end Add_Note_Text;

      begin
         Scan (List, Add_Note_Item'Access);
      end Add_Note_Items;

   begin
      for Note of List_Table.Element (List).List loop
         declare
            use type Ada.Containers.Count_Type;
            Name  : constant Name_Id := Get_Name (Note_Name (Note));
            Value : constant Node_Id := Note_Value (Note);
            Value_List : constant List_Id :=
                           Node_Table.Element (Value).List;
         begin

            Have_Single_Item := List_Table (Value_List).List.Length = 1;
            Have_Single_Integer := False;

            Add_Note_Items (To_Standard_String (Name), Value_List);

            if To_Standard_String (Name) = "aqua_modular_type" then
               if Have_Single_Integer then
                  declare
                     Modulus : Positive;
                  begin
                     Modulus := Single_Integer;
                     Class.Set_Modulus (Modulus);
                     Ack.Generate.Primitives
                       .Create_Integral_Primitives (Class);
                  end;
               else
                  raise Constraint_Error with
                  Get_Program (Value).Show_Location
                    & ": positive integer required for modulus bits";
               end if;
            end if;
         end;
      end loop;
   end Analyse_Notes;

end Ack.Semantic.Analysis.Classes;
