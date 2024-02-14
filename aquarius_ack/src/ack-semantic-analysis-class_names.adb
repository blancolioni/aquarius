with Ada.Text_IO;

with Ack.Environment;

with Ack.Semantic.Classes;

package body Ack.Semantic.Analysis.Class_Names is

   function Scan_Name
     (Referrer       : Aquarius.Programs.Program_Tree;
      Start_Position : List_Of_Nodes.Cursor;
      End_Position   : List_Of_Nodes.Cursor;
      Start_Parent   : Ack.Classes.Class_Entity)
      return Ack.Classes.Class_Entity;

   ------------------------
   -- Analyse_Class_Name --
   ------------------------

   procedure Analyse_Class_Name
     (Context       : Ack.Classes.Class_Entity;
      Class_Name    : Node_Id;
      Defining_Name : Boolean)
   is
      use Ack.Classes;
      --  use type List_Of_Nodes.Cursor;
      List : constant List_Of_Nodes.List :=
               List_Table.Element (Identifiers (Class_Name)).List;
      Position : List_Of_Nodes.Cursor := List.First;
      Class_Context : Class_Entity := Context;
      Last : constant List_Of_Nodes.Cursor :=
               (if Defining_Name
                then List.Last
                else List_Of_Nodes.No_Element);

      Parent        : Ack.Classes.Class_Entity;
      Referrer : constant Aquarius.Programs.Program_Tree :=
                        Get_Program (Class_Name);
   begin

      if not Defining_Name
        and then Natural (List.Length) = 1
      then
         --  this might be a local type, not a class
         declare
            Local_Node : constant Node_Id := List.First_Element;
            Local_Name : constant Name_Id := Get_Name (Local_Node);
         begin
            if Context.Contains (Local_Name) then
               if Has_Entity (Class_Name) then
                  raise Constraint_Error with
                  Get_Program (Local_Node).Show_Location
                    & "node already has an entity: "
                    & Get_Entity (Class_Name).Description;
               end if;
               Set_Entity (Class_Name, Context.Get (Local_Name));
               return;
            end if;
         end;
      end if;

      if Defining_Name then
         Parent := null;
      else
         Parent := null;
         declare
            Element_Node : constant Node_Id :=
                             List_Of_Nodes.Element (Position);
            Element_Name : constant Name_Id :=
                             Node_Table.Element (Element_Node).Name;
         begin
            while Class_Context /= null loop
               Parent :=
                 Ack.Semantic.Classes.Load_Class
                   (Referrer, Class_Context, Element_Name);
               exit when Parent /= null;
               Class_Context := Class_Context.Class_Declaration_Context;
            end loop;
            if Parent = null then
               Parent :=
                 Ack.Semantic.Classes.Load_Class
                   (Referrer, Ack.Environment.Top_Level, Element_Name);
            end if;
         end;

         if Parent = null then
            Error (List_Of_Nodes.Element (Position), E_Undeclared_Name);
            return;
         end if;

         List_Of_Nodes.Next (Position);

      end if;

      Parent := Scan_Name (Referrer, Position, Last, Parent);

      --
      --  while Position /= Last loop
      --     declare
      --        Element_Node : constant Node_Id :=
      --                         List_Of_Nodes.Element (Position);
      --        Element_Name : constant Name_Id :=
      --                         Node_Table.Element (Element_Node).Name;
      --        New_Parent   : constant Class_Entity :=
      --                         Ack.Semantic.Classes.Load_Class
      --                           (Referrer        => Referrer,
      --                            Parent          =>
      --                              (if Parent = null
      --                               then Ack.Environment.Top_Level
      --                               else Parent),
      --                            Name            => Element_Name);
      --     begin
      --        if New_Parent = null then
      --           Error (Element_Node,
      --                  (if Parent = null
      --                   then E_Undeclared_Name
      --                   else E_No_Child));
      --           Parent :=
      --             Ack.Classes.New_Class
      --               (Element_Name, Parent, Element_Node);
      --        else
      --           Parent := New_Parent;
      --        end if;
      --     end;
      --     List_Of_Nodes.Next (Position);
      --  end loop;

      if Defining_Name then
         declare
            Last_Node : constant Node_Id :=
                          List_Of_Nodes.Element (Last);
            Last_Name : constant Name_Id :=
                          Node_Table.Element (Last_Node).Name;
            New_Class : constant Class_Entity :=
                          Ack.Classes.New_Class
                            (Last_Name, Parent, Last_Node);
         begin
            --  if Parent = null then
               --  Ack.Environmentironment.Top_Level.Insert (New_Class);
            --  else
               --  Parentarent.Insert (New_Class);
            --  end if;
            Set_Entity (Last_Node, New_Class);
            Parent := New_Class;
         exception
            when others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Get_Program (Last_Node).Show_Location
                  & ": unhandled exception while defining "
                  & To_String (Last_Name));
               raise;
         end;
      end if;

      if Has_Entity (Class_Name) then
         Ada.Text_IO.Put_Line
           (Get_Program (Class_Name).Show_Location
            & ": cannot set entity to "
            & Parent.Identity & " " & Parent.Qualified_Name);
         Ada.Text_IO.Put_Line
           (Get_Program (Class_Name).Show_Location
            & ": already contains entity "
            & Get_Entity (Class_Name).Identity
            & " " & Get_Entity (Class_Name).Description);
      end if;

      Set_Entity (Class_Name, Parent);

   end Analyse_Class_Name;

   ---------------
   -- Scan_Name --
   ---------------

   function Scan_Name
     (Referrer       : Aquarius.Programs.Program_Tree;
      Start_Position : List_Of_Nodes.Cursor;
      End_Position   : List_Of_Nodes.Cursor;
      Start_Parent   : Ack.Classes.Class_Entity)
      return Ack.Classes.Class_Entity
   is
      use type List_Of_Nodes.Cursor;
      Position : List_Of_Nodes.Cursor := Start_Position;
      Parent   : Ack.Classes.Class_Entity := Start_Parent;
   begin
      while Position /= End_Position loop
         declare
            use Ack.Classes;
            Element_Node : constant Node_Id :=
                             List_Of_Nodes.Element (Position);
            Element_Name : constant Name_Id :=
              Node_Table.Element (Element_Node).Name;
            Parent_Env   : constant Entity_Type :=
              (if Parent = null then Ack.Environment.Top_Level
               else Entity_Type (Parent));
            New_Parent   : constant Class_Entity :=
                             Ack.Semantic.Classes.Load_Class
                               (Referrer        => Referrer,
                                Parent          => Parent_Env,
                                Name            => Element_Name);
         begin
            if New_Parent = null then
               Error (Element_Node,
                      (if Parent = null
                       then E_Undeclared_Name
                       else E_No_Child));
               Parent :=
                 Ack.Classes.New_Class
                   (Element_Name, Parent, Element_Node);
            else
               Parent := New_Parent;
            end if;
         end;
         List_Of_Nodes.Next (Position);
      end loop;
      return Parent;

   end Scan_Name;

end Ack.Semantic.Analysis.Class_Names;
