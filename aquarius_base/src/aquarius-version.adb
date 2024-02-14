with Ada.Strings.Fixed;

package body Aquarius.Version is

   Build_Number     : constant := 1001;

   function Image (Value : Integer) return String;

   -----------
   -- Image --
   -----------

   function Image (Value : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (Value),
                                     Ada.Strings.Left);
   end Image;

   --------------------
   -- Version_String --
   --------------------

   function Version_String return String is
   begin
      return Name & " version " & Image (Version_Major) &
        "." & Image (Version_Minor) & "." & Image (Version_Release) &
        "." & Image (Build_Number) &
        " (" & Release_Name & ")";
   end Version_String;

end Aquarius.Version;
