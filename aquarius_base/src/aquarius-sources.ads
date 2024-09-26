package Aquarius.Sources is

   type Source_Interface is interface;
   --  Interface for referencing the various sources of program text

   type Source_Reference is access all Source_Interface'Class;

   function Short_Name (This : Source_Interface) return String
                        is abstract;
   --  A short name that refers to this source.
   --  For example, if the source is on the file system, this
   --  could be the simple name of the file, without the path.

   function Full_Name (This : Source_Interface) return String
                       is abstract;
   --  The full name of the source.  This should be enough to
   --  globally identify the location.
   --  For example, the full path to a file.

   function Grammar_Tag (This : Source_Interface) return String
                         is abstract;
   --  A tag from the source which links it to a grammar.
   --  For example, a file extension.

   function Internal_Source
     (Name : String;
      Tag  : String)
      return Source_Reference;
   --  Create a source for internal use.

end Aquarius.Sources;
