package Aquarius.Regexes is

   type Regex is private;

   function Is_Null (Ex : Regex) return Boolean;

   --  Match: match the given Ex to Text.
   --    Last -> the index of the last character matched
   --    Partial -> true if Text (1 .. Last) represents a
   --               partial match
   --    Complete -> true if Text (1 .. Last) represents a
   --               complete match
   procedure Match (Text     : String;
                    Ex       : Regex;
                    Last     :    out Natural;
                    Partial  :    out Boolean;
                    Complete :    out Boolean);

   function Image (Ex : Regex) return String;

private

   type Regex_Record;
   type Regex is access Regex_Record;

   type Character_Match is array (Character) of Boolean;

   function Null_Regex return Regex;
   function Any_Character_Match return Regex;
   function New_Character_Match (Match : Character_Match) return Regex;

   function New_Repeat (Ex            : Regex;
                        At_Least_Once : Boolean)
                       return Regex;

   function New_Optional (Ex : Regex) return Regex;

   function New_Sequence (Left, Right : Regex) return Regex;

end Aquarius.Regexes;
