package body Aquarius.UI is

   type Null_Command_Instance is
     new Command_Interface with null record;

   overriding function Tag
     (This : Null_Command_Instance)
      return String
   is ("null-command");

   overriding function Description (This : Null_Command_Instance) return String
   is ("A command that does nothing");

   overriding procedure Execute (This : Null_Command_Instance) is null;
   overriding procedure Undo (This : Null_Command_Instance) is null;

   function Null_Command return Command_Interface'Class
   is (Null_Command_Instance'(null record));

end Aquarius.UI;
