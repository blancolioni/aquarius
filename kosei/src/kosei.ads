package Kosei is

   type Configuration_Interface is interface;

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

   function Get
     (Path : String)
      return String;

   procedure Add_Configuration
     (This : Configuration_Interface'Class);

   procedure Add_Configuration
     (This : Configuration_Interface'Class;
      Path : String);

end Kosei;
