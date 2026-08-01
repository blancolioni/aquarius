with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;

with Interfaces;

package body Aquarius.Devices.Real_Handler is

   use Aqua;
   use type Interfaces.IEEE_Float_64;

   subtype Real is Interfaces.IEEE_Float_64;

   package Real_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real);

   function To_Real is new Ada.Unchecked_Conversion (Word_64, Real);
   function To_Bits is new Ada.Unchecked_Conversion (Real, Word_64);

   Register_Count : constant := 5;
   type Register_Index is range 0 .. Register_Count - 1;

   R_Command          : constant Register_Index := 0;
   R_Argument_1_High  : constant Register_Index := 1;
   R_Argument_1_Low   : constant Register_Index := 2;
   R_Argument_2_High  : constant Register_Index := 3;
   R_Argument_2_Low   : constant Register_Index := 4;

   type Register_Array is array (Register_Index) of Word_32;

   Command_No_Operation : constant := 0;
   Command_Reciprocal   : constant := 1;
   Command_Sqrt         : constant := 2;
   Command_Ln           : constant := 3;
   Command_Exp          : constant := 4;
   Command_Sin          : constant := 5;
   Command_Cos          : constant := 6;
   Command_Tan          : constant := 7;
   Command_Arcsin       : constant := 8;
   Command_Arccos       : constant := 9;
   Command_Arctan       : constant := 10;
   Command_Arctan2      : constant := 11;

   Error_No_Error         : constant := 0;
   Error_Invalid_Argument : constant := 1;
   Error_Invalid_Command  : constant := 2;

   subtype Parent is Aquarius.Devices.Instance;
   type Instance is new Parent with
      record
         Rs     : Register_Array := [others => 0];
      end record;

   type Instance_Reference is access all Instance'Class;

   overriding function Name (This : Instance) return String
   is ("aqua-real-handler");

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
      Arg_1 : constant Real :=
                To_Real (Word_64 (This.Rs (R_Argument_1_High)) * 2 ** 32
                         + Word_64 (This.Rs (R_Argument_1_Low)));
      Arg_2 : constant Real :=
                To_Real (Word_64 (This.Rs (R_Argument_2_High)) * 2 ** 32
                         + Word_64 (This.Rs (R_Argument_2_Low)));

      procedure Set_Result (Value : Real);
      procedure Set_Error (Error : Word_32);

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error (Error : Word_32) is
      begin
         This.Rs (R_Command) := Error;
      end Set_Error;

      ----------------
      -- Set_Result --
      ----------------

      procedure Set_Result (Value : Real) is
         Bits : constant Word_64 := To_Bits (Value);
      begin
         This.Rs (R_Argument_1_High) := Word_32 (Bits / 2 ** 32);
         This.Rs (R_Argument_1_Low) := Word_32 (Bits mod 2 ** 32);
      end Set_Result;

   begin
      This.Rs (R_Command) := Error_No_Error;

      case Command is
         when Command_No_Operation =>
            null;
         when Command_Reciprocal =>
            if Arg_1 = 0.0 then
               Set_Error (Error_Invalid_Argument);
            else
               Set_Result (1.0 / Arg_1);
            end if;
         when Command_Sqrt =>
            if Arg_1 < 0.0 then
               Set_Error (Error_Invalid_Argument);
            else
               Set_Result (Real_Functions.Sqrt (Arg_1));
            end if;
         when Command_Ln =>
            if Arg_1 <= 0.0 then
               Set_Error (Error_Invalid_Argument);
            else
               Set_Result (Real_Functions.Log (Arg_1));
            end if;
         when Command_Exp =>
            Set_Result (Real_Functions.Exp (Arg_1));
         when Command_Sin =>
            Set_Result (Real_Functions.Sin (Arg_1));
         when Command_Cos =>
            Set_Result (Real_Functions.Cos (Arg_1));
         when Command_Tan =>
            Set_Result (Real_Functions.Tan (Arg_1));
         when Command_Arcsin =>
            if abs Arg_1 > 1.0 then
               Set_Error (Error_Invalid_Argument);
            else
               Set_Result (Real_Functions.Arcsin (Arg_1));
            end if;
         when Command_Arccos =>
            if abs Arg_1 > 1.0 then
               Set_Error (Error_Invalid_Argument);
            else
               Set_Result (Real_Functions.Arccos (Arg_1));
            end if;
         when Command_Arctan =>
            Set_Result (Real_Functions.Arctan (Arg_1));
         when Command_Arctan2 =>
            if Arg_1 = 0.0 and then Arg_2 = 0.0 then
               Set_Error (Error_Invalid_Argument);
            else
               Set_Result (Real_Functions.Arctan (Arg_1, Arg_2));
            end if;
         when others =>
            Set_Error (Error_Invalid_Command);
      end case;

   exception
      when Ada.Numerics.Argument_Error | Constraint_Error =>
         Set_Error (Error_Invalid_Argument);
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

end Aquarius.Devices.Real_Handler;
