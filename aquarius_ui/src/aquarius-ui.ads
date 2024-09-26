package Aquarius.UI is

   type Element_Interface is interface;

   function Tag (This : Element_Interface) return String is abstract;

   type Command_Interface is interface and Element_Interface;

   function Description (This : Command_Interface) return String is abstract;
   procedure Execute (This : Command_Interface) is abstract;
   procedure Undo (This : Command_Interface) is abstract;

   function Null_Command return Command_Interface'Class;

end Aquarius.UI;
