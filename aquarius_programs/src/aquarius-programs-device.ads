with Aqua;
with Aquarius.Devices;

package Aquarius.Programs.Device is

   function To_Address (Tree : Program_Tree) return Aqua.Address_Type;

   function Aquarius_Tree_Driver return Aquarius.Devices.Reference;

private

   Register_Count : constant := 256;
   type Register_Index is range 0 .. Register_Count - 1;

   R_Current      : constant Register_Index := 0;
   R_Command      : constant Register_Index := 1;
   R_Transfer     : constant Register_Index := 2;
   R_String_Len   : constant Register_Index := 3;
   R_String       : constant Register_Index := 4;

   type Register_Array is array (Register_Index) of Aqua.Word_32;

   type Aquarius_Tree_Driver_Record is
     new Aquarius.Devices.Instance with
      record
         Current : Program_Tree;
         Rs      : Register_Array := [others => 0];
      end record;

   overriding function Name
     (This : Aquarius_Tree_Driver_Record)
      return String
   is ("aquarius-program-tree-driver");

   overriding function Word_Count
     (This : Aquarius_Tree_Driver_Record)
      return Natural
   is (Register_Count);

   overriding procedure Get_Word_32
     (This    : in out Aquarius_Tree_Driver_Record;
      Address : Aqua.Address_Type;
      Value   : out Aqua.Word_32);

   overriding procedure Set_Word_32
     (This    : in out Aquarius_Tree_Driver_Record;
      Address : Aqua.Address_Type;
      Value   : Aqua.Word_32);

   procedure Execute_Command
     (This : in out Aquarius_Tree_Driver_Record'Class);

   procedure Write_String
     (Driver : in out Aquarius_Tree_Driver_Record'Class;
      S      : String);

   function Read_String
     (Driver : Aquarius_Tree_Driver_Record'Class)
      return String;

   function To_Address (Tree : Program_Tree) return Aqua.Address_Type
   is (Aqua.Address_Type (Tree.Sequence));

end Aquarius.Programs.Device;
