with WL.String_Maps;

with Aquarius.Plugins.Aqua_Plugin;
with Aquarius.Plugins.EBNF;

package body Aquarius.Plugins.Manager is

   package Plugin_Maps is
     new WL.String_Maps (Aquarius.Plugins.Reference);

   Loaded_Plugins    : Plugin_Maps.Map;
   Local_EBNF_Plugin : Reference;

   ----------------
   -- Get_Plugin --
   ----------------

   function Get_Plugin (Name : String) return Aquarius.Plugins.Reference is
   begin
      if Loaded_Plugins.Contains (Name) then
         return Loaded_Plugins.Element (Name);
      end if;

      if Name = "ebnf" then
         return Local_EBNF_Plugin;
      else
         raise Constraint_Error with
           Name & ": no such plugin";
      end if;
   end Get_Plugin;

   ----------
   -- Load --
   ----------

   procedure Load (From_Grammar : Aquarius.Grammars.Aquarius_Grammar) is
      Name   : constant String := From_Grammar.Name;
      Plugin : Reference;
   begin
      if Loaded_Plugins.Contains (Name) then
         return; -- n Loaded_Plugins.Element (To_Plugin_Map_Name (Name));
      end if;

      if Name = "ebnf" then
         if Local_EBNF_Plugin = null then
            Local_EBNF_Plugin := new Aquarius.Plugins.EBNF.Instance;
         end if;
         Plugin := Local_EBNF_Plugin;
      else
         Plugin := new Aquarius.Plugins.Aqua_Plugin.Instance;
      end if;

      Plugin.Load (Name);

      Loaded_Plugins.Insert (Name, Plugin);
   end Load;

   --------------------------
   -- Loaded_Plugin_Report --
   --------------------------

   procedure Loaded_Plugin_Report is
   begin
      for Plugin of Loaded_Plugins loop
         Plugin.Report_State;
      end loop;
   end Loaded_Plugin_Report;

end Aquarius.Plugins.Manager;
