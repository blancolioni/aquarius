with Aquarius.Grammars.Manager;

with WL.Unit.Compare_Test;

package body Aquarius.Tests.Grammar is

   function Id (S : String) return String is (S);

   function Get_Json_Grammar return String;

   package Grammar_Tests is
      new WL.Unit.Compare_Test (String, Id, Compare);

   ----------------------
   -- Get_Json_Grammar --
   ----------------------

   function Get_Json_Grammar return String is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar_For_File
                    (File_Name => "a/b/c.json");
   begin
      return Grammar.Name;
   end Get_Json_Grammar;

   ----------
   -- Load --
   ----------

   procedure Load (Suite : in out WL.Unit.Test_Suite) is
   begin
      Suite.Append
        (Grammar_Tests.Test
           ("get-json-grammar", Get_Json_Grammar'Access, "json"));
   end Load;

end Aquarius.Tests.Grammar;
