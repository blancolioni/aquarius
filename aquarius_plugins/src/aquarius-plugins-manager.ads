with Aquarius.Grammars;

package Aquarius.Plugins.Manager is

   function Load
     (From_Grammar : Aquarius.Grammars.Aquarius_Grammar)
      return Boolean;

   function Get_Plugin (Name : String) return Aquarius.Plugins.Reference;

   procedure Loaded_Plugin_Report;

end Aquarius.Plugins.Manager;
