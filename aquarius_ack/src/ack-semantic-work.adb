with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;

with Ack.Errors;

with Ack.Semantic.Analysis.Features;

package body Ack.Semantic.Work is

   Trace_Work : constant Boolean := False;

   type Work_Item is
      record
         Category  : Work_Item_Category;
         Class     : Ack.Classes.Class_Entity;
         Feature   : Node_Id;
      end record;

   function Image (Item : Work_Item) return String;
   procedure Execute_Work_Item (Item : Work_Item);

   package Work_Item_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Work_Item);

   Work_List : Work_Item_Lists.List;

   function Category_Image (Category : Work_Item_Category) return String
   is (case Category is
          when Feature_Header => "feature-header",
          when Feature_Body   => "feature-body",
          when Class_Binding  => "class-binding",
          when Class_Layout   => "class-layout",
          when Error_Report   => "error-report");

   -------------------
   -- Add_Work_Item --
   -------------------

   procedure Add_Work_Item
     (Category  : Work_Item_Category;
      Class     : Ack.Classes.Class_Entity;
      Feature   : Node_Id)
   is
      New_Item : constant Work_Item := (Category, Class, Feature);
   begin
      Work_List.Append (New_Item);

      if Trace_Work then
         Ada.Text_IO.Put_Line ("add work item: " & Image (New_Item));
      end if;

   end Add_Work_Item;

   ---------------------
   -- Check_Work_Item --
   ---------------------

   procedure Check_Work_Item
     (Class        : not null access constant
        Ack.Classes.Class_Entity_Record'Class;
      Feature_Name : Name_Id;
      Category     : Work_Item_Category)
   is
      use Work_Item_Lists;

      Finished : Boolean := False;

      function Match_Feature (Feature_Node : Node_Id) return Boolean;

      -------------------
      -- Match_Feature --
      -------------------

      function Match_Feature (Feature_Node : Node_Id) return Boolean is
         Names : constant List_Id := New_Feature_List (Feature_Node);
      begin
         for Node of List_Table.Element (Names).List loop
            declare
               Extended_Name_Node : constant Node_Id :=
                                      Extended_Feature_Name (Node);
               Name_Node          : constant Node_Id :=
                                      Ack.Feature_Name (Extended_Name_Node);
               Name               : constant Name_Id := Get_Name (Name_Node);
            begin
               if Name = Feature_Name then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Match_Feature;

   begin

      if Trace_Work then
         Ada.Text_IO.Put_Line
           ("check work item: "
            & Class.Qualified_Name
            & (if Feature_Name = No_Name then ""
              else "." & To_Standard_String (Feature_Name))
            & ": "
            & (Category_Image (Category)));
      end if;

      while not Finished loop
         declare
            Position : Cursor := No_Element;
         begin
            for Search_Cursor in Work_List.Iterate loop
               declare
                  Item : constant Work_Item :=
                           Element (Search_Cursor);
               begin
                  if Item.Class.Qualified_Name = Class.Qualified_Name then
                     exit when Item.Category > Category;

                     if Feature_Name = No_Name
                       or else Item.Feature = No_Node
                       or else Match_Feature (Item.Feature)
                     then
                        Position := Search_Cursor;
                        exit;
                     end if;
                  end if;
               end;
            end loop;

            if Has_Element (Position) then
               declare
                  Item : constant Work_Item := Element (Position);
               begin
                  Work_List.Delete (Position);
                  Execute_Work_Item (Item);
                  Finished := Item.Category = Category;
               end;
            else
               Finished := True;
            end if;
         end;
      end loop;
   end Check_Work_Item;

   ------------------
   -- Execute_Work --
   ------------------

   procedure Execute_Work is
      Item : constant Work_Item := Work_List.First_Element;
   begin
      Work_List.Delete_First;
      Execute_Work_Item (Item);
   end Execute_Work;

   -----------------------
   -- Execute_Work_Item --
   -----------------------

   procedure Execute_Work_Item (Item : Work_Item) is
   begin

      if Trace_Work then
         Ada.Text_IO.Put_Line ("executing: " & Image (Item));
      end if;

      case Item.Category is
         when Feature_Header =>
            Ack.Semantic.Analysis.Features.Analyse_Feature_Header
              (Item.Class, Item.Feature);
         when Feature_Body =>
            Ack.Semantic.Analysis.Features.Analyse_Feature_Body
              (Item.Class, Item.Feature);
         when Class_Binding =>
            Item.Class.Bind;
         when Class_Layout =>
            Item.Class.Create_Memory_Layout;
         when Error_Report =>
            Ack.Errors.Record_Errors
              (Item.Class.Top_Class_Node);
            Ack.Errors.Report_Errors
              (Item.Class.Top_Class_Node);
      end case;
   end Execute_Work_Item;

   ---------------
   -- Have_Work --
   ---------------

   function Have_Work return Boolean is
   begin
      return not Work_List.Is_Empty;
   end Have_Work;

   -----------
   -- Image --
   -----------

   function Image (Item : Work_Item) return String is
   begin
      return Category_Image (Item.Category)
        & ": "
        & (if Item.Feature = No_Node
           then Item.Class.Qualified_Name
           else Get_Program (Item.Feature).Show_Location);
   end Image;

end Ack.Semantic.Work;
