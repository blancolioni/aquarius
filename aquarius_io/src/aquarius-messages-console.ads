package Aquarius.Messages.Console is

   procedure Show_Messages (List : Message_List);

   function Check_Messages
     (Source : Message_Location'Class;
      Minimum : Message_Level := Warning)
      return Message_Level;
   --  check Source for messages, and report any with a level > Minimum.
   --  return the highest message level found.
   --  if no messages found, No_Message is returned.

end Aquarius.Messages.Console;
