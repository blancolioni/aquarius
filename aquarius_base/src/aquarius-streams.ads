with Aquarius.Locations;

package Aquarius.Streams is

   type Stream_Offset is new Natural;

   type Stream_Reader_Interface is interface
     and Aquarius.Locations.Location_Interface;

   type Reader_Reference is access all Stream_Reader_Interface'Class;

   procedure Close
     (This : not null access Stream_Reader_Interface)
   is abstract;

   function End_Of_Stream
     (This : Stream_Reader_Interface)
      return Boolean
      is abstract;

   procedure Get_Line
     (This        : in out Stream_Reader_Interface;
      Line        : out String;
      Last        : out Natural)
   is abstract;

   function Current_Offset
     (This : Stream_Reader_Interface)
      return Stream_Offset
      is abstract;

   function Get_Line
     (This : in out Stream_Reader_Interface'Class)
      return String;

   type Stream_Writer_Interface is interface;
   type Writer_Reference is access all Stream_Writer_Interface'Class;

   procedure Close
     (This : not null access Stream_Writer_Interface)
   is abstract;

   function To_String
     (This : Stream_Writer_Interface)
      return String
      is abstract;

   procedure Put
     (This : in out Stream_Writer_Interface;
      Text : String)
   is abstract;

   procedure New_Line
     (This : in out Stream_Writer_Interface)
   is abstract;

end Aquarius.Streams;
