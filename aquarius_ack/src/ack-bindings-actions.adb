package body Ack.Bindings.Actions is

   --------------
   -- Add_Tree --
   --------------

   procedure Add_Tree
     (Table     : in out Ack_Binding_Table;
      Tree_Name : String)
   is
   begin
      if not Table.Map.Contains (Tree_Name) then
         Table.Map.Insert (Tree_Name, Child_Binding_Maps.Empty_Map);
      end if;
   end Add_Tree;

   --------------------
   -- Create_Binding --
   --------------------

   procedure Create_Binding
     (Table             : in out Ack_Binding_Table;
      Parent_Tree_Name  : String;
      Child_Tree_Name   : String;
      Position          : Binding_Position)
   is
      Child_Key : constant String :=
                    Child_Tree_Name
                    & "--"
                    & (case Position is
                          when Before => "before",
                          when After  => "after");

   begin
      if not Table.Map.Contains (Parent_Tree_Name) then
         Table.Map.Insert (Parent_Tree_Name, Child_Binding_Maps.Empty_Map);
      end if;

      if not Table.Map.Element (Parent_Tree_Name).Contains (Child_Key) then
         Table.Map (Parent_Tree_Name).Insert
           (Child_Key, (Get_Name_Id (Child_Tree_Name), Position));
      end if;

   end Create_Binding;

   ------------------
   -- Have_Binding --
   ------------------

   function Have_Binding
     (Table             : Ack_Binding_Table;
      Parent_Tree_Name  : String;
      Child_Tree_Name   : String;
      Position          : Binding_Position)
      return Boolean
   is
      Child_Key : constant String :=
                    Child_Tree_Name
                    & "--"
                    & (case Position is
                          when Before => "before",
                          when After  => "after");
   begin
      return Table.Map.Contains (Parent_Tree_Name)
        and then Table.Map (Parent_Tree_Name).Contains (Child_Key);
   end Have_Binding;

   ----------------
   -- Scan_Trees --
   ----------------

   procedure Scan_Trees
     (Table : Ack_Binding_Table;
      Process : not null access
        procedure (Tree_Name : String))
   is
   begin
      for Position in Table.Map.Iterate loop
         Process (Binding_Maps.Key (Position));
      end loop;
   end Scan_Trees;

end Ack.Bindings.Actions;
