package Aquarius.UI.Commands is

   type Command_Manager_Interface is interface;

   procedure Append_Command
     (This    : in out Command_Manager_Interface;
      Command : Command_Interface'Class)
   is abstract;

   procedure Next_Command
     (This    : in out Command_Manager_Interface)
   is abstract;

   procedure Previous_Command
     (This    : in out Command_Manager_Interface)
   is abstract;

   function Create_Command_Manager return Command_Manager_Interface'Class;

end Aquarius.UI.Commands;
