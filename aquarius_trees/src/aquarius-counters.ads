package Aquarius.Counters is

   type Counter_Type is private;

   function Show (Counter : Counter_Type) return String;

   type Counter_Source is tagged limited private;

   function Next (Source : in out Counter_Source) return Counter_Type;

private

   Counter_Length : constant := 4;

   type Counter_Type is new String (1 .. Counter_Length);

   type Counter_Source is tagged limited
      record
         Current : Counter_Type := "1A1A";
      end record;

end Aquarius.Counters;
