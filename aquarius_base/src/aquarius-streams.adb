package body Aquarius.Streams is

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This : in out Stream_Reader_Interface'Class)
      return String
   is
      Buffer : String (1 .. 200);
      Last   : Natural;
   begin
      This.Get_Line (Buffer, Last);
      if Last = Buffer'Last then
         return Buffer & This.Get_Line;
      else
         return Buffer (1 .. Last);
      end if;
   end Get_Line;

end Aquarius.Streams;
