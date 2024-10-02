with Aquarius.Grammars;
with Aquarius.Programs;

package Aquarius.UI.Editor is

   type View_Interface is interface;

   type Model_Interface is interface;
   type Model_Reference is access all Model_Interface'Class;

   function Insert_Character_At_Point
     (This : not null access Model_Interface;
      Ch   : Character)
      return Command_Interface'Class
      is abstract;

   function Contents
     (This : Model_Interface)
      return String
      is abstract;

   type Model_Factory_Interface is interface;

   function Create
     (This    : Model_Factory_Interface;
      Grammar : Aquarius.Grammars.Aquarius_Grammar;
      Program : Aquarius.Programs.Program_Tree)
      return Model_Reference
   is abstract;

end Aquarius.UI.Editor;
