private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Aqua;

private with Aqua.CPU;
private with Aqua.OS;

package Aquarius.Plugins.Dynamic is

   type Dynamic_Plugin_Type is
     new Aquarius_Plugin_Type
   with private;

   overriding
   function Name (Plugin : Dynamic_Plugin_Type) return String;

   overriding
   function Version (Plugin : Dynamic_Plugin_Type) return String;

   overriding
   procedure Load (Plugin  : not null access Dynamic_Plugin_Type;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar);

   overriding procedure Report_State
     (Plugin : Dynamic_Plugin_Type);

   procedure Load_Object
     (Plugin : in out Dynamic_Plugin_Type'Class;
      Name   : String);

   function New_Dynamic_Plugin
     (Name    : String;
      Version : String)
     return Aquarius_Plugin;

   procedure Load_Action_Bindings
     (This              : Dynamic_Plugin_Type;
      Binding_File_Path : String;
      Aqua_Source_Path  : String;
      Grammar           : Aquarius.Grammars.Aquarius_Grammar;
      Group             : Aquarius.Actions.Action_Group);

   type Dynamic_Plugin is access all Dynamic_Plugin_Type'Class;

   procedure Execute
     (This  : Dynamic_Plugin_Type'Class;
      Start : Aqua.Address_Type;
      Args  : Aqua.Array_Of_Words);

private

   package Defined_Property_Vectors is
      new Ada.Containers.Vectors (Positive,
                                  Aquarius.Properties.Property_Type,
                                  Aquarius.Properties."=");

   type Dynamic_Plugin_Access is access all Dynamic_Plugin_Type'Class;

   type Dynamic_Plugin_Type is
     new Aquarius_Plugin_Type
       with
      record
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Version    : Ada.Strings.Unbounded.Unbounded_String;
         Plugin     : Aquarius.Properties.Property_Type;
         Properties : Defined_Property_Vectors.Vector;
         CPU        : Aqua.CPU.Reference;
         OS         : Aqua.OS.Reference;
      end record;

end Aquarius.Plugins.Dynamic;
