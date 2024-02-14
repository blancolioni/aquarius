package Aquarius.Layout is

   pragma Preelaborate (Aquarius.Layout);

   type Count is new Natural;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type Position_Offset is new Integer;

   subtype Position is
     Position_Offset range 0 .. Position_Offset'Last;

   function Show (Pos : Position) return String;

   function Fill (Text    : String;
                  Width   : Positive;
                  Justify : Boolean)
                  return String;

   --  Fill: split the text into lines so that it fills the given
   --  width.  If Justify is true, the right margin will be
   --  justified.

--     type Region is
--        record
--           Start  : Position;
--           Finish : Position;
--        end record;

   type Line_Offset is new Integer;
   subtype Line_Number is Line_Offset range 1 .. Line_Offset'Last;

   type Column_Offset is new Integer;
   subtype Column_Number is Column_Offset range 1 .. Column_Offset'Last;

end Aquarius.Layout;
