with Ada.Text_IO;

package body Aquarius.Devices.Text_Writer is

   use Aqua;

   Register_Count : constant := 64;
   type Register_Index is range 0 .. Register_Count - 1;

   R_Character        : constant Register_Index := 0;
   R_Output           : constant Register_Index := 1;
   R_File_Name_Length : constant Register_Index := 2;
   R_File_Name_Start  : constant Register_Index := 3;

   type Register_Array is array (Register_Index) of Word_32;

   subtype Parent is Aquarius.Devices.Instance;

   type Instance is new Parent with
      record
         Rs     : Register_Array := [others => 0];
      end record;

   overriding function Name (This : Instance) return String
   is ("aqua-text-writer");

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

   ------------
   -- Create --
   ------------

   function Create
     return Aquarius.Devices.Reference
   is
   begin
      return new Instance;
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
      case R is
         when R_Character =>
            Ada.Text_IO.Put (Character'Val (Value));
         when R_Output =>
            null;
         when R_File_Name_Length =>
            null;
         when R_File_Name_Start =>
            null;
         when others =>
            null;
      end case;
   end Set_Word_32;

end Aquarius.Devices.Text_Writer;
