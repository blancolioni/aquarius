with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;

package body Kosei is

   package Configuration_Holders is
     new Ada.Containers.Indefinite_Holders
       (Configuration_Interface'Class);

   type Configuration_Record (Name_Length : Natural) is
      record
         Name   : String (1 .. Name_Length);
         Config : Configuration_Holders.Holder;
      end record;

   package Configuration_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Configuration_Record);

   Config_List : Configuration_Lists.List;

   function Match_Path
     (Path        : String;
      Config_Path : String)
      return String;
   --  If Path starts with Config_Path, return the
   --  rest of Path after removing the matching characters.
   --  If it does not match, return an empty string

   function Iterate_Path
     (Start : Cursor_Interface'Class;
      Path  : String)
      return String;

   -----------------------
   -- Add_Configuration --
   -----------------------

   procedure Add_Configuration
     (This : Configuration_Interface'Class)
   is
   begin
      Add_Configuration (This, "");
   end Add_Configuration;

   -----------------------
   -- Add_Configuration --
   -----------------------

   procedure Add_Configuration
     (This : Configuration_Interface'Class;
      Path : String)
   is
      Full_Path : constant String :=
                    (if Path = ""
                     or else Path (Path'First) /= '/'
                     then "/" & Path
                     else Path);
   begin
      Config_List.Append
        (Configuration_Record'
           (Full_Path'Length, Full_Path,
            Configuration_Holders.To_Holder (This)));
   end Add_Configuration;

   ---------
   -- Get --
   ---------

   function Get
     (Path : String)
      return String
   is
   begin
      for Element of Config_List loop
         declare
            Index_Path : constant String :=
                           Match_Path (Path, Element.Name);
         begin
            if Index_Path /= "" then
               return Element.Config.Element.Root.Iterate_Path (Index_Path);
            end if;
         end;
      end loop;
      return "";
   end Get;

   ------------------
   -- Iterate_Path --
   ------------------

   function Iterate_Path
     (Start : Cursor_Interface'Class;
      Path  : String)
      return String
   is
   begin
      for I in Path'Range loop
         if Path (I) = '/' then
            return Start
              .Element (Path (Path'First .. I - 1))
                .Iterate_Path (Path (I + 1 .. Path'Last));
         end if;
      end loop;

      return Start.Value (Path);
   end Iterate_Path;

   ----------------
   -- Match_Path --
   ----------------

   function Match_Path
     (Path        : String;
      Config_Path : String)
      return String
   is
   begin
      if Path'Length > Config_Path'Length then
         declare
            First : constant Positive := Path'First;
            Last  : constant Natural  := First + Config_Path'Length - 1;
            Head  : constant String :=
                      Path (First .. Last);
         begin
            if Head = Config_Path then
               return Path (Last + 1 .. Path'Last);
            end if;
         end;
      end if;
      return "";
   end Match_Path;

end Kosei;
