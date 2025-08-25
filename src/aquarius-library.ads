package Aquarius.Library is

   Configuration_Error : exception;

   procedure Initialize;
   procedure Shut_Down;

   function Configuration_Path return String;

end Aquarius.Library;
