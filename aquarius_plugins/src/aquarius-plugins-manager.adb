with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Plugins.EBNF;
with Aquarius.Plugins.Script_Plugin;

with Aquarius.Configuration;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;

package body Aquarius.Plugins.Manager is

   subtype Plugin_Map_Name is String (1 .. 16);

   package Plugin_Map is
      new Ada.Containers.Hashed_Maps (Plugin_Map_Name,
                                      Aquarius_Plugin,
                                      Ada.Strings.Fixed.Hash,
                                      "=");

   Loaded_Plugins : Plugin_Map.Map;

   Local_EBNF_Plugin     : Aquarius_Plugin;
   Local_Script_Plugin   : Aquarius_Plugin;

   function To_Plugin_Map_Name
     (Name : String)
     return Plugin_Map_Name;

   ----------------
   -- Get_Plugin --
   ----------------

   function Get_Plugin (Name : String) return Aquarius_Plugin is
   begin
      if Loaded_Plugins.Contains (To_Plugin_Map_Name (Name)) then
         return Loaded_Plugins.Element (To_Plugin_Map_Name (Name));
      end if;
      if Name = "ebnf" then
         return Local_EBNF_Plugin;
      elsif Name = "script" then
         return Local_Script_Plugin;
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
      Plugin : Aquarius_Plugin;
   begin
      if Loaded_Plugins.Contains (To_Plugin_Map_Name (Name)) then
         return; -- n Loaded_Plugins.Element (To_Plugin_Map_Name (Name));
      end if;

      if Name = "ebnf" then
         if Local_EBNF_Plugin = null then
            Local_EBNF_Plugin := new Aquarius.Plugins.EBNF.EBNF_Plugin;
         end if;
         Plugin := Local_EBNF_Plugin;
      elsif Name = "script" then
         if Local_Script_Plugin = null then
            Local_Script_Plugin :=
              new Aquarius.Plugins.Script_Plugin.Script_Plugin_Type;
         end if;
         Plugin := Local_Script_Plugin;
      else
         declare
            Path : constant String :=
                     Ada.Directories.Compose
                       (Aquarius.Configuration.Grammar_Path (Name),
                        Name & ".plugin");
         begin
            if not Ada.Directories.Exists (Path) then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "warning: grammar " & Name &
                                       " has no associated plugin");
               return;
            else
               declare
                  Script_Grammar : constant Grammars.Aquarius_Grammar :=
                    Aquarius.Grammars.Manager.Get_Grammar ("script");
                  Script : Aquarius.Programs.Program_Tree;
               begin
                  Load (Script_Grammar);
                  Script :=
                    Aquarius.Loader.Load_From_File (Script_Grammar, Path);
                  Plugin := Aquarius.Plugins.Script_Plugin.Get_Plugin (Script);
               end;
            end if;
         end;
      end if;

      Ada.Text_IO.Put_Line ("Plugin: " & Plugin.Name & " version " &
                              Plugin.Version);

      Loaded_Plugins.Insert (To_Plugin_Map_Name (Name), Plugin);

      Plugin.Load (From_Grammar);
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

   ------------------------
   -- To_Plugin_Map_Name --
   ------------------------

   function To_Plugin_Map_Name
     (Name : String)
     return Plugin_Map_Name
   is
      Result : Plugin_Map_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Result,
                              Drop => Ada.Strings.Right);
      return Result;
   end To_Plugin_Map_Name;

end Aquarius.Plugins.Manager;
