with Ack.Bindings;

with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Actions;
with Aquarius.Configuration;
with Aquarius.Devices.Meta;
with Aquarius.Grammars.Manager;
with Aquarius.Options;
with Aquarius.Programs;
with Aquarius.Syntax;

with Kosei.Json;

package body Aquarius.Plugins.Aqua_Plugin is

   type Aqua_Action_Instance is
     new Aquarius.Actions.Action_Execution_Interface with
      record
         Server    : Aqua.Server.Reference;
         Start     : Aqua.Address_Type;
      end record;

   overriding procedure Execute
     (Executor : Aqua_Action_Instance;
      Item     : not null access Aquarius.Actions.Actionable'Class);

   overriding procedure Execute
     (Executor : Aqua_Action_Instance;
      Parent   : not null access Aquarius.Actions.Actionable'Class;
      Child    : not null access Aquarius.Actions.Actionable'Class);

   function Create_Server
     return Aqua.Server.Reference;

   -----------------
   -- Bind_Action --
   -----------------

   procedure Bind_Action
     (This        : in out Instance;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Parent_Name : String;
      Child_Name  : String;
      Address     : Aqua.Address_Type)
   is
      Exec : constant Aqua_Action_Instance :=
               Aqua_Action_Instance'
                 (This.Server, Address);
      Parent : constant Aquarius.Syntax.Syntax_Tree :=
               This.Grammar.Get_Definition (Parent_Name);
      Child : constant Aquarius.Syntax.Syntax_Tree :=
                (if Child_Name = "" then null
                 else This.Grammar.Get_Definition (Child_Name));
   begin
      if Child_Name = "" then
         Parent.Set_Action
           (Group    => Group,
            Position => Position,
            Action   => Exec);
      else
         Parent.Set_Action
           (Child    => Child,
            Group    => Group,
            Position => Position,
            Action   => Exec);
      end if;
   end Bind_Action;

   -------------------
   -- Create_Server --
   -------------------

   function Create_Server
     return Aqua.Server.Reference
   is
   begin
      return Server : constant Aqua.Server.Reference :=
           Aqua.Server.Create
          (Aquarius.Configuration.Aqua_Configuration_Path,
           Aquarius.Configuration.Object_Path)
      do
         Server.Install_Device
           (Base   => 16#FFFF_F200#,
            Bound  => 16#FFFF_F300#,
            Device => Aquarius.Devices.Meta.Create (Server));
      end return;
   end Create_Server;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Aqua_Action_Instance;
      Item     : not null access Aquarius.Actions.Actionable'Class)
   is
      Node    : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Programs.Program_Tree (Item);
      Top     : constant Aquarius.Programs.Program_Tree :=
                  Node.Program_Root;
      Top_Seq : constant Natural := Top.Sequence_Number;
      Seq     : constant Natural := Node.Sequence_Number;
   begin
      Executor.Server.Run
        (Start => Executor.Start,
         Arguments => [0,
                       Aqua.Word_32 (Top_Seq),
                       Aqua.Word_32 (Seq)],
         Trace     => Aquarius.Options.Aqua_Trace);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Aqua_Action_Instance;
      Parent   : not null access Aquarius.Actions.Actionable'Class;
      Child    : not null access Aquarius.Actions.Actionable'Class)
   is
      Parent_Tree : constant Aquarius.Programs.Program_Tree :=
               Aquarius.Programs.Program_Tree (Parent);
      Parent_Seq  : constant Natural := Parent_Tree.Sequence_Number;
      Top_Tree    : constant Aquarius.Programs.Program_Tree :=
                      Parent_Tree.Program_Root;
      Top_Seq     : constant Natural := Top_Tree.Sequence_Number;
      Child_Tree  : constant Aquarius.Programs.Program_Tree :=
                      Aquarius.Programs.Program_Tree (Child);
      Child_Seq  : constant Natural := Child_Tree.Sequence_Number;
   begin
      --  Ada.Text_IO.Put_Line (Top_Tree.Image);
      --  Ada.Text_IO.Put_Line (Parent_Tree.Image);
      --  Ada.Text_IO.Put_Line (Child_Tree.Image);

      Executor.Server.Run
        (Start     => Executor.Start,
         Arguments => [0,
                       Aqua.Word_32 (Top_Seq),
                       Aqua.Word_32 (Parent_Seq),
                       Aqua.Word_32 (Child_Seq)],
         Trace     => Aquarius.Options.Aqua_Trace);
   end Execute;

   ----------
   -- Load --
   ----------

   overriding function Load
     (This : in out Instance;
      Name : String)
      return Boolean
   is
      Plugin_Dir : constant String :=
                     Aquarius.Configuration.Grammar_Path (Name);
      Plugin_File : constant String :=
                      Ada.Directories.Compose (Plugin_Dir, Name, "json");
      Grammar     : constant Aquarius.Grammars.Aquarius_Grammar :=
                      Aquarius.Grammars.Manager.Get_Grammar (Name);
      Kosei_Path  : constant String := "/plugins/" & Name;
      Success     : Boolean := True;
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

      This.Server.Load
        (Aquarius.Configuration.Object_File_Path
           ("aquarius-init"));
      This.Server.Run;

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
                  Success := False;
                  return;
               end if;

               declare

                  Object_Path : constant String :=
                                  Configuration.Object_File_Path
                                    (Binding_Name);

                  procedure Process_Note
                    (Name        : String;
                     Tag         : Aqua.Word_32;
                     Description : String);

                  ------------------
                  -- Process_Note --
                  ------------------

                  procedure Process_Note
                    (Name        : String;
                     Tag         : Aqua.Word_32;
                     Description : String)
                  is
                     pragma Unreferenced (Tag);
                     Start : Positive := Description'First;
                     function Next_Token return String;

                     ----------------
                     -- Next_Token --
                     ----------------

                     function Next_Token return String is
                        Index : Positive := Start;
                     begin
                        if Start > Description'Last then
                           return "";
                        end if;

                        while Index <= Description'Last
                          and then Description (Index) /= ' '
                        loop
                           Index := Index + 1;
                        end loop;
                        return S : constant String :=
                          Description (Start .. Index - 1)
                        do
                           Start := Index + 1;
                        end return;
                     end Next_Token;

                  begin
                     if Name = "bind_action" then
                        declare
                           Group_Name : constant String := Next_Token;
                           Link_Name   : constant String := Next_Token;
                           Moment      : constant String := Next_Token;
                           Parent      : constant String := Next_Token;
                           Child       : constant String := Next_Token;
                           Address     : constant Aqua.Address_Type :=
                                           This.Server.Get_Symbol_Address
                                             (Link_Name);
                        begin
                           This.Bind_Action
                             (Group       =>
                                This.Action_Groups.Element (Group_Name),
                              Position    => Rule_Position'Value (Moment),
                              Parent_Name => Parent,
                              Child_Name  => Child,
                              Address     => Address);
                        end;
                     end if;
                  end Process_Note;

               begin
                  This.Server.Load
                    (Object_Path,
                     Process_Note'Access);
               end;
            end;
         end Load_Group;

      begin
         Kosei.Get (Kosei_Path & "/groups")
           .Iterate_Children (Load_Group'Access);
      end;

      return Success;

   end Load;

end Aquarius.Plugins.Aqua_Plugin;
