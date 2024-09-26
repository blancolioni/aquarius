package Aquarius.Locations is

   type Location_Offset is new Natural;

   type Line_Count is new Natural;
   subtype Line_Index is Line_Count range 1 .. Line_Count'Last;

   type Column_Count is new Natural;
   subtype Column_Index is Column_Count range 1 .. Column_Count'Last;

   type Location_Interface is interface;

   function Offset
     (This : Location_Interface)
      return Location_Offset
      is abstract;

   function Line
     (This : Location_Interface)
      return Line_Index
      is abstract;

   function Column
     (This : Location_Interface)
      return Column_Index
      is abstract;

   function Before
     (This  : Location_Interface'Class;
      Other : not null access constant Location_Interface'Class)
      return Boolean;

   type Updateable_Location_Interface is interface and Location_Interface;

   procedure Update_Location
     (This : in out Updateable_Location_Interface;
      From : Location_Interface'Class)
      is abstract;

   procedure Update_Location
     (This   : in out Updateable_Location_Interface'Class;
      Offset : Location_Offset;
      Line   : Line_Index;
      Column : Column_Index);

   procedure Update_Column
     (This       : in out Updateable_Location_Interface'Class;
      New_Column : Column_Index);

   type Instance is new Updateable_Location_Interface with private;

   overriding function Offset
     (This : Instance)
      return Location_Offset;

   overriding function Line
     (This : Instance)
      return Line_Index;

   overriding function Column
     (This : Instance)
      return Column_Index;

   overriding procedure Update_Location
     (This : in out Instance;
      From : Location_Interface'Class);

   function To_Location
     (Offset : Location_Offset;
      Line   : Line_Index;
      Column : Column_Index)
      return Instance;

private

   type Instance is new Updateable_Location_Interface with
      record
         Offset   : Location_Offset := 0;
         Line     : Line_Index      := 1;
         Column   : Column_Index    := 1;
      end record;

   overriding function Offset
     (This : Instance)
      return Location_Offset
   is (This.Offset);

   overriding function Line
     (This : Instance)
      return Line_Index
   is (This.Line);

   overriding function Column
     (This : Instance)
      return Column_Index
   is (This.Column);

   function Before
     (This  : Location_Interface'Class;
      Other : not null access constant Location_Interface'Class)
      return Boolean
   is (This.Offset < Other.Offset);

   function To_Location
     (Offset : Location_Offset;
      Line   : Line_Index;
      Column : Column_Index)
      return Instance
   is (Instance'(Offset, Line, Column));

end Aquarius.Locations;
