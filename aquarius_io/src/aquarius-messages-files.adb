with Ada.Text_IO;

package body Aquarius.Messages.Files is

   -------------------
   -- Show_Messages --
   -------------------

   procedure Save_Messages
     (Log_File_Path : String;
      List          : Message_List)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Log_File_Path);
      for I in 1 .. Message_Count (List) loop
         declare
            Msg : constant Message := Get_Message (List, I);
         begin
            Put_Line (File, Show (Msg));
            for J in 1 .. Reference_Count (Msg) loop
               Put_Line (File, Show_Reference (Msg, J));
            end loop;
         end;
      end loop;
      Close (File);
   end Save_Messages;

end Aquarius.Messages.Files;
