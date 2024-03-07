with Ack.Bindings;

with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Configuration;
with Aquarius.Devices.Meta;
with Aquarius.Grammars.Manager;

with Kosei.Json;

package body Aquarius.Plugins.Aqua_Plugin is

   function Create_Server
     return Aqua.Server.Reference;

   procedure Load_Object_File
     (Server : Aqua.Server.Reference;
      Name   : String);

   -------------------
   -- Create_Server --
   -------------------

   function Create_Server
     return Aqua.Server.Reference
   is
   begin
      return Server : constant Aqua.Server.Reference :=
        Aqua.Server.Create
          ("./share/aqua_vm/aqua.config",
           "./.aquarius/tmp/obj")
      do
         Server.Install_Device
           (Base   => 16#FFFF_F200#,
            Bound  => 16#FFFF_F300#,
            Device => Aquarius.Devices.Meta.Create (Server));
      end return;
   end Create_Server;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (This : in out Instance;
      Name : String)
   is
      Plugin_Dir : constant String :=
                     Aquarius.Configuration.Grammar_Path (Name);
      Plugin_File : constant String :=
                      Ada.Directories.Compose (Plugin_Dir, Name, "json");
      Grammar     : constant Aquarius.Grammars.Aquarius_Grammar :=
                      Aquarius.Grammars.Manager.Get_Grammar (Name);
      Kosei_Path  : constant String := "/plugins/" & Name;
   begin
      Kosei.Json.Add_Json_Config (Plugin_File, Kosei_Path);

      This.Grammar := Grammar;
      This.Name :=
        Aquarius.Names.To_Aquarius_Name
          (Kosei.Get (Kosei_Path & "/name"));
      This.Description :=
        Aquarius.Names.To_Aquarius_Name
          (Kosei.Get (Kosei_Path & "/description"));
      This.Version :=
        Aquarius.Names.To_Aquarius_Name
          (Kosei.Get (Kosei_Path & "/version"));

      Ada.Text_IO.Put_Line ("Loaded " & Name & " version "
                            & Aquarius.Names.To_String (This.Version));
      Ada.Text_IO.Put_Line (Aquarius.Names.To_String (This.Description));

      This.Server := Create_Server;

      declare
         procedure Load_Group (Position : Kosei.Cursor_Interface'Class);

         ----------------
         -- Load_Group --
         ----------------

         procedure Load_Group (Position : Kosei.Cursor_Interface'Class) is
            Trigger : Aquarius.Actions.Action_Execution_Trigger;
            Stage   : constant String := Position.Value ("stage");
         begin
            begin
               Trigger :=
                 Aquarius.Actions.Action_Execution_Trigger'Value
                   (Stage & "_Trigger");
            exception
               when Constraint_Error =>
                  raise Constraint_Error with
                  Stage & ": no such trigger";
            end;

            declare
               Group : constant Aquarius.Actions.Action_Group :=
                         This.New_Action_Group
                           (Name    => Position.Value ("name"),
                            Trigger => Trigger);
               Binding_Name : constant String :=
                                Name
                                & "-"
                                & Aquarius.Actions.Action_Group_Name (Group)
                              & "-"
                                & "action_bindings";
               Loaded : Boolean;
            begin
               --  pragma Unreferenced (Group);
               Ada.Text_IO.Put_Line
                 ("new action group: "
                  & Aquarius.Actions.Action_Group_Name (Group)
                  & ": " & Trigger'Image);
               Loaded :=
                 Ack.Bindings.Load_Ack_Binding
                   (Binding_File_Path =>
                      Aquarius.Configuration.Generated_File_Path
                        (Binding_Name & ".aqua"),
                    Base_Aqua_Path    => Plugin_Dir & "/aqua",
                    Grammar           => This.Grammar,
                    Group             => Group);
               if not Loaded then
                  Ada.Text_IO.Put_Line
                    ("Failed to load "
                     & Aquarius.Actions.Action_Group_Name (Group));
               end if;

               Load_Object_File (This.Server, Binding_Name);

            end;
         end Load_Group;

      begin
         Kosei.Get (Kosei_Path & "/groups")
           .Iterate_Children (Load_Group'Access);
      end;

   end Load;

   ----------------------
   -- Load_Object_File --
   ----------------------

   procedure Load_Object_File
     (Server : Aqua.Server.Reference;
      Name   : String)
   is
      Object_Path : constant String :=
                      Aquarius.Configuration.Object_File_Path (Name);
   begin
      Server.Load (Object_Path);
   end Load_Object_File;

end Aquarius.Plugins.Aqua_Plugin;
