with Ada.Wide_Wide_Characters.Handling;

package body Aquarius.Devices.Character_Handler is

   use Aqua;

   Register_Count : constant := 2;
   type Register_Index is range 0 .. Register_Count - 1;

   R_Command       : constant Register_Index := 0;
   R_Character     : constant Register_Index := 1;

   type Register_Array is array (Register_Index) of Word_32;

   Command_To_Upper       : constant := 1;
   Command_To_Lower       : constant := 2;
   Command_Is_White_Space : constant := 3;

   subtype Parent is Aquarius.Devices.Instance;
   type Instance is new Parent with
      record
         Rs     : Register_Array := [others => 0];
      end record;

   type Instance_Reference is access all Instance'Class;

   overriding function Name (This : Instance) return String
   is ("aqua-character-handler");

   overriding function Word_Count (This : Instance) return Natural
   is (Natural (Register_Count));

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32);

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32);

   procedure Run_Command
     (This    : in out Instance'Class;
      Command : Word_32);

   ------------
   -- Create --
   ------------

   function Create
     return Aquarius.Devices.Reference
   is
      This : constant Instance_Reference := new Instance;
   begin
      return Aquarius.Devices.Reference (This);
   end Create;

   -----------------
   -- Get_Word_32 --
   -----------------

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32)
   is
   begin
      Value := This.Rs (Register_Index (Address / 4));
   end Get_Word_32;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (This    : in out Instance'Class;
      Command : Word_32)
   is
      type Converter is access
        function (Ch : Wide_Wide_Character) return Wide_Wide_Character;
      type Predicate is access
        function (Ch : Wide_Wide_Character) return Boolean;

      procedure Apply (Conv : Converter);
      procedure Apply (Pred : Predicate);

      -----------
      -- Apply --
      -----------

      procedure Apply (Conv : Converter) is
         Ch : constant Wide_Wide_Character :=
                Conv (Wide_Wide_Character'Val (This.Rs (R_Character)));
      begin
         This.Rs (R_Character) := Wide_Wide_Character'Pos (Ch);
      end Apply;

      -----------
      -- Apply --
      -----------

      procedure Apply (Pred : Predicate) is
         Result : constant Boolean :=
                Pred (Wide_Wide_Character'Val (This.Rs (R_Character)));
      begin
         This.Rs (R_Character) := Boolean'Pos (Result);
      end Apply;

   begin
      case Command is
         when Command_To_Upper =>
            Apply (Ada.Wide_Wide_Characters.Handling.To_Upper'Access);
         when Command_To_Lower =>
            Apply (Ada.Wide_Wide_Characters.Handling.To_Lower'Access);
         when Command_Is_White_Space =>
            Apply (Ada.Wide_Wide_Characters.Handling.Is_Space'Access);
         when others =>
            null;
      end case;
   end Run_Command;

   -----------------
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32)
   is
      R : constant Register_Index := Register_Index (Address / 4);
   begin
      This.Rs (R) := Value;
      if R = R_Command then
         This.Run_Command (Value);
      end if;
   end Set_Word_32;

end Aquarius.Devices.Character_Handler;
