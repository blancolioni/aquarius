with Ada.Containers.Doubly_Linked_Lists;

package body Aquarius.Free_Lists is

   package Name_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Name);

   Name_List : Name_Lists.List;

   -----------
   -- Claim --
   -----------

   function Claim return Name is
   begin
      if Name_List.Is_Empty then
         return N : constant Name := new Object do
            null;
         end return;
      else
         return N : constant Name := Name_List.First_Element do
            Name_List.Delete_First;
         end return;
      end if;
   end Claim;

   -------------
   -- Release --
   -------------

   procedure Release (X : Name) is
   begin
      Name_List.Append (X);
   end Release;

end Aquarius.Free_Lists;
