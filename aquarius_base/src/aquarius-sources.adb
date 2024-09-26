package body Aquarius.Sources is

   type Instance (Name_Length, Tag_Length : Natural) is
     new Source_Interface with
      record
         Name : String (1 .. Name_Length);
         Tag  : String (1 .. Tag_Length);
      end record;

   overriding function Short_Name (This : Instance) return String
   is (This.Name);

   overriding function Full_Name (This : Instance) return String
   is (This.Name);

   overriding function Grammar_Tag (This : Instance) return String
   is (This.Tag);

   ---------------------
   -- Internal_Source --
   ---------------------

   function Internal_Source
     (Name : String;
      Tag  : String)
      return Source_Reference
   is
   begin
      return new Instance'(Name'Length, Tag'Length, Name, Tag);
   end Internal_Source;

end Aquarius.Sources;
