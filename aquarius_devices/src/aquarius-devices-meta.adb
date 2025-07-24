with Ada.Text_IO;
with Aqua.Images;

package body Aquarius.Devices.Meta is

   use Aqua;

   type Register_Index is range 0 .. 63;

   Command          : constant Register_Index := 0;
   Result           : constant Register_Index := 1;
   Device_Id_Length : constant Register_Index := 4;
   Device_Id_Start  : constant Register_Index := 5;

   type Register_Array is array (Register_Index) of Word_32;

   subtype Parent is Aquarius.Devices.Instance;

   type Instance is new Parent with
      record
         Server : Aqua.Server.Reference;
         Top    : Address_Type   := 16#FFFF_0000#;
         Rs     : Register_Array := [others => 0];
      end record;

   overriding function Name (This : Instance) return String
   is ("meta");

   overriding function Word_Count
     (This : Instance)
      return Natural
   is (64);

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32);

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32);

   procedure Install_Device (This : in out Instance'Class);

   ------------
   -- Create --
   ------------

   function Create
     (Server : Aqua.Server.Reference)
      return Aqua.Devices.Reference
   is
      Device : constant Aqua.Devices.Reference := new Instance'
        (Parent with Server => Server, others => <>);
   begin
      return Device;
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

   --------------------
   -- Install_Device --
   --------------------

   procedure Install_Device (This : in out Instance'Class) is
      Length : constant Natural := Natural (This.Rs (Device_Id_Length));
      Id     : String (1 .. Length);
      Index  : Register_Index := Device_Id_Start;
   begin
      for Ch of Id loop
         Ch := Character'Val (This.Rs (Index));
         Index := Index + 1;
      end loop;
      declare
         Device : constant Aquarius.Devices.Reference :=
                    Aquarius.Devices.Get (Id);
         Base   : constant Address_Type := This.Top;
         Bound  : constant Address_Type :=
                    This.Top + Word_32 (Device.Word_Count * 4);
      begin
         Ada.Text_IO.Put_Line
           ("installing: " & Id
            & " at " & Aqua.Images.Hex_Image (Base)
            & ":" & Aqua.Images.Hex_Image (Bound));

         This.Server.Install_Device
           (Base, Bound, Aqua.Devices.Reference (Device));

         This.Rs (Result) := This.Top;
         This.Top := Bound;
      end;

   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (Id & ": no such device");

   end Install_Device;

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
      if R = Command and then Value /= 0 then
         This.Install_Device;
      else
         This.Rs (Register_Index (Address / 4)) := Value;
      end if;
   end Set_Word_32;

end Aquarius.Devices.Meta;
