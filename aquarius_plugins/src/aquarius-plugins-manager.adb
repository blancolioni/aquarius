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

   function Load
     (From_Grammar : Aquarius.Grammars.Aquarius_Grammar)
      return Boolean
   is
      Name   : constant String := From_Grammar.Name;
      Plugin : Reference;
   begin
      if Loaded_Plugins.Contains (Name) then
         return True;
      end if;

      if Name = "ebnf" then
         if Local_EBNF_Plugin = null then
            Local_EBNF_Plugin := new Aquarius.Plugins.EBNF.Instance;
         end if;
         Plugin := Local_EBNF_Plugin;
      else
         Plugin := new Aquarius.Plugins.Aqua_Plugin.Instance;
      end if;

      if Plugin.Load (Name) then
         Loaded_Plugins.Insert (Name, Plugin);
         return True;
      else
         return False;
      end if;
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
