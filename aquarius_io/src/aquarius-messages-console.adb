with Ada.Text_IO;

package body Aquarius.Messages.Console is

   --------------------
   -- Check_Messages --
   --------------------

   function Check_Messages
     (Source : Message_Location'Class;
      Minimum : Message_Level := Warning)
      return Message_Level
   is
      List : Message_List;
      Highest : Message_Level := No_Message;
   begin
      Source.Get_Messages (List);
      if Message_Count (List) > 0 then
         Highest := Highest_Level (List);
         if Highest >= Minimum then
            Aquarius.Messages.Console.Show_Messages (List);
         end if;
      end if;
      return Highest;
   end Check_Messages;

   -------------------
   -- Show_Messages --
   -------------------

   procedure Show_Messages (List : Message_List) is
   begin
      for I in 1 .. Message_Count (List) loop
         declare
            Msg : constant Message := Get_Message (List, I);
         begin
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  Show (Msg));
            for J in 1 .. Reference_Count (Msg) loop
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     Show_Reference (Msg, J));
            end loop;
         end;
      end loop;
   end Show_Messages;

end Aquarius.Messages.Console;
