with Ada.Strings.Maps;

package Aquarius.Lexers is

   --  General lexing functions

   --  Lexer: a machine that accepts a single character
   type Lexer is private;

   function Matches_New_Line
     (Lex : Lexer)
      return Boolean;
   --  Return True if Lex can consume the end of line condition

   function Null_Lexer return Lexer;
   --  Null_Lexer is a lexer that doesn't match anything

   --  joining lexers
   function "&" (Left, Right : Lexer) return Lexer;
   function "or" (Left, Right : Lexer) return Lexer;

   function "not" (Left : Lexer) return Lexer;
   --  not is only for single rule lexers: it negates the rule.

   function Repeat (Item : Lexer) return Lexer;
   --  Repetition (zero or more times)
   --  For zero or more, use Optional (Repeat (...))

   function Optional (Item : Lexer) return Lexer;
   --  Optional

   --  Scanning particular characters or sets of characters
   function Letter       return Lexer;
   function Lowercase    return Lexer;
   function Uppercase    return Lexer;
   function Digit        return Lexer;
   function Hex_Digit    return Lexer;
   function Alphanumeric return Lexer;
   function Graphic      return Lexer;
   function Any          return Lexer;

   function End_Of_Line  return Lexer;

   function Literal (Ch : Character) return Lexer;
   function In_Range (Low, High : Character) return Lexer;

   function One_Of (S : String) return Lexer;
   --  Convenience function for a lexer that recognises
   --  exactly one of the characters in S

   function Sequence (S : String) return Lexer;
   --  Convenience function for a lexer that recognises
   --  a sequence of characters given by S

   function Symbol_Lexer (S : String) return Lexer;
   --  Symbol_Lexer creates a lexer that parses the longest
   --  sequence of characters that matches one of the given
   --  space-separated symbols

   function Run (Lex    : Lexer;
                 Text   : String)
                return Natural;
   --  Run Lex on text, and return an index one greater than that
   --  of the final matching character.
   --  Return zero if nothing matches.

   function Start (Lex : Lexer)
                  return Ada.Strings.Maps.Character_Set;

   function Overlaps
     (Lex_1, Lex_2 : Lexer)
      return Boolean;
   --  Return True if the two lexers overlap each other
   --  (i.e. there exists at least one string which matches both)
   --  (to be honest: check if the start character sets overlap)

   function Show (Lex : Lexer) return String;

private

   type Lexer_Type is (Terminal, Sequence, Repeat, Optional, Choice, Negate);

   type Lexer_Rule_Type is (Built_In, Condition,
                            Single_Character, Character_Range,
                            Or_Rule);

   type Lexer_State is (End_Of_Line, End_Of_File);

   type Lexer_Rule;
   type Lexer_Rule_Access is access Lexer_Rule;

   type Lexer_Rule (Rule_Type : Lexer_Rule_Type) is
      record
         Negate : Boolean;
         case Rule_Type is
            when Built_In =>
               Fn : not null access
                 function (Ch : Character) return Boolean;
            when Condition =>
               State : Lexer_State;
            when Single_Character =>
               Match : Character;
            when Character_Range =>
               Lo, Hi : Character;
            when Or_Rule =>
               Left, Right : Lexer_Rule_Access;
         end case;
      end record;

   type Lexer_Node;
   type Lexer is access Lexer_Node;

   type Lexer_Node (Node_Type : Lexer_Type) is
      record
         case Node_Type is
            when Terminal =>
               Rule        : Lexer_Rule_Access;
            when Sequence =>
               First       : Lexer;
               Rest        : Lexer;
            when Repeat | Optional =>
               Child       : Lexer;
            when Choice =>
               Left, Right : Lexer;
            when Negate =>
               Negated     : Lexer;
         end case;
      end record;

end Aquarius.Lexers;
