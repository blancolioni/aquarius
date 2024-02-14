with Aqua.OS;

with Aquarius.Actions;
with Aquarius.Grammars;

package Ack.Bindings is

   function Load_Ack_Binding
     (Binding_File_Path : String;
      Base_Aqua_Path    : String;
      OS                : Aqua.OS.Reference;
      Grammar           : Aquarius.Grammars.Aquarius_Grammar;
      Group             : Aquarius.Actions.Action_Group)
      return Boolean;

private

   type Binding_Position is (Before, After);

end Ack.Bindings;
