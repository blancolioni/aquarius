with Ack.Bindings;

with Aqua.Bus;
with Aqua.Linker;
with Aqua.Loader;

with Aquarius.Configuration;
with Aquarius.Grammars.Manager;

package body Aquarius.Plugins.Dynamic is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This  : Dynamic_Plugin_Type'Class;
      Start : Aqua.Address_Type;
      Args  : Aqua.Array_Of_Words)
   is
   begin
      This.CPU.Start (Start, Args);
   end Execute;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Plugin  : not null access Dynamic_Plugin_Type;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Aquarius_Plugin_Type (Plugin.all).Load (Grammar);
   end Load;

   --------------------------
   -- Load_Action_Bindings --
   --------------------------

   procedure Load_Action_Bindings
     (This              : Dynamic_Plugin_Type;
      Binding_File_Path : String;
      Aqua_Source_Path  : String;
      Grammar           : Aquarius.Grammars.Aquarius_Grammar;
      Group             : Aquarius.Actions.Action_Group)
   is
      Success : constant Boolean :=
                  Ack.Bindings.Load_Ack_Binding
                    (Binding_File_Path => Binding_File_Path,
                     Base_Aqua_Path    => Aqua_Source_Path,
                     OS                => This.OS,
                     Grammar           => Grammar,
                     Group             => Group);
   begin
      pragma Assert (Success);
   end Load_Action_Bindings;

   -----------------
   -- Load_Object --
   -----------------

   procedure Load_Object
     (Plugin : in out Dynamic_Plugin_Type'Class;
      Name   : String)
   is
   begin
      Aqua.Linker.Load
        (OS   => Plugin.OS,
         Path =>
           Aquarius.Configuration.Object_File_Path (Name));
   end Load_Object;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Plugin : Dynamic_Plugin_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Plugin.Name);
   end Name;

   ------------------------
   -- New_Dynamic_Plugin --
   ------------------------

   function New_Dynamic_Plugin
     (Name    : String;
      Version : String)
     return Aquarius_Plugin
   is
      Result : constant Dynamic_Plugin_Access := new Dynamic_Plugin_Type;
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Manager.Get_Grammar (Name);
      Bus     : constant Aqua.Bus.Reference := Aqua.Bus.Create_Bus;
      CPU     : constant Aqua.CPU.Reference := Aqua.Loader.Create_CPU (Bus);
      OS      : constant Aqua.OS.Reference := Aqua.OS.Create (Bus);
   begin
      CPU.Attach_Memory_Manager (OS);
      Result.Name     := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result.Version  := Ada.Strings.Unbounded.To_Unbounded_String (Version);
      Result.CPU := CPU;
      Aquarius_Plugin_Type'Class (Result.all).Load (Grammar);
      return Aquarius_Plugin (Result);
   end New_Dynamic_Plugin;

   ------------------
   -- Report_State --
   ------------------

   overriding procedure Report_State
     (Plugin : Dynamic_Plugin_Type)
   is
   begin
      null;
   end Report_State;

   -------------
   -- Version --
   -------------

   overriding function Version
     (Plugin : Dynamic_Plugin_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Plugin.Version);
   end Version;

end Aquarius.Plugins.Dynamic;
