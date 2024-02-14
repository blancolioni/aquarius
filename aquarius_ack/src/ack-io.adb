--  with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Text_IO;

--  with Aquarius.Config_Paths;

package body Ack.IO is

   ---------
   -- Put --
   ---------

   procedure Put (Node : Node_Id) is
      use Ada.Text_IO;
      Current_Indent : constant Count := Col;
   begin
      if Node = No_Node then
         Put ("<>");
      elsif Node = Error_Node then
         Put ("#error");
      else
         declare
            Rec : Node_Record renames Node_Table (Node);
         begin
            Put (Node_Kind'Image (Rec.Kind));
            if Rec.Name /= No_Name then
               Put (" [" & Name_Table.Element (Rec.Name) & "]");
            end if;
            for Field of Rec.Field loop
               if Field /= No_Node then
                  New_Line;
                  Set_Col (Current_Indent + 2);
                  Put (Field);
               end if;
            end loop;
            if Rec.List /= No_List then
               for Element of List_Table (Rec.List).List loop
                  New_Line;
                  Set_Col (Current_Indent + 2);
                  Put (Element);
               end loop;
            end if;
         end;
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Node : Node_Id) is
   begin
      Put (Node);
      Ada.Text_IO.New_Line;
   end Put_Line;

end Ack.IO;
