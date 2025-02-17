package Kosei is

   type Configuration_Interface is interface;

   procedure Put (This : Configuration_Interface) is abstract;

   type Cursor_Interface is interface;

   function Root
     (This : Configuration_Interface)
      return Cursor_Interface'Class
      is abstract;

   function Element
     (Position : Cursor_Interface;
      Name     : String)
      return Cursor_Interface'Class
      is abstract;

   function Value
     (Position : Cursor_Interface;
      Name     : String)
      return String
      is abstract;

   procedure Iterate_Children
     (This    : Cursor_Interface;
      Process : not null access
        procedure (Position : Cursor_Interface'Class))
   is abstract;

   function Get
     (Path : String)
      return String;

   function Get
     (Path : String)
      return Cursor_Interface'Class;

   procedure Add_Configuration
     (This : Configuration_Interface'Class);

   procedure Add_Configuration
     (This : Configuration_Interface'Class;
      Path : String);

end Kosei;
