--  with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Maps;

with Aquarius.Lexers;

package Aquarius.Tokens is

   Scan_Error : exception;
   --  something went wrong while scanning

   Token_Error : exception;
   --  something went wrong while defining tokens

   --  Token_Text: stores dynamically created but thereafter
   --  constant Strings which have no trailing spaces.
   type Token_Text is private;

   function To_Token_Text (Item : String) return Token_Text;
   function To_String (Item : Token_Text) return String;

   function "=" (Left  : Token_Text;
                 Right : String)
                return Boolean;
   function "=" (Left  : String;
                 Right : Token_Text)
                 return Boolean;

   overriding
   function "=" (Left, Right : Token_Text) return Boolean;

   --  Token: a single lexical element in a grammar
   type Token is private;

   Null_Token : constant Token;

   --  Class_Token: represents a set of tokens with
   --  identical lexical rules.  For example, reserved
   --  words generally belong in the 'identifier' token
   --  class.
   type Token_Class is private;

   --  Token_Frame: all tokens of a particular grammar belong
   --  to a unique frame.
   type Token_Frame is private;

   type Array_Of_Tokens is array (Positive range <>) of Token;

   function Terminals (Frame : Token_Frame) return Array_Of_Tokens;
   --  return an array containing all terminal tokens of this frame

   --  Token_Set: a set of tokens
   type Token_Set is private;

   function Empty return Token_Set;
   function "<=" (Left   : Token;
                  Right  : Token_Set)
                 return Boolean;
   procedure Add (Tok    :        Token;
                  To_Set : in out Token_Set);

   function New_Frame
     (Case_Sensitive : Boolean)
     return Token_Frame;

   procedure Set_Case_Sensitive (Frame : Token_Frame;
                                 Value : Boolean);

   --  Is_Reserved: some tokens are assigned a special meaning by the
   --  language.  For example, in the following line of code we have
   --  the reserved tokens 'type' 'is' 'private' and ';'.  Each
   --  reserved token belongs to a class, from which it derives its
   --  lexical rules.  The tokens 'type', 'private' and 'is' belong to
   --  the identifier token class; ';' belongs to the class of single
   --  character delimiters.

   function Is_Reserved (Frame : Token_Frame;
                         Tok   : Token)
                        return Boolean;

   --  Is_Class_Token: returns True for any token which has an
   --  arbitrary text component taken from the file source.  For
   --  example, identifiers, strings, character and number literals
   --  all have source text.  In constract, reserved tokens generally
   --  have text which is specified by the language definition.

   function Is_Class_Token (Frame : Token_Frame;
                            Tok   : Token) return Boolean;

   --  Get_Class_Token: return the unique token that matches
   --  against source text of the class that isn't reserved.
   --  For example, the class token for the identifier token
   --  class is returned by Scan whenever identifier text is
   --  parsed that doesn't correspond to a particular reserved
   --  word.
   function Get_Class_Token (Frame : Token_Frame;
                             Class : Token_Class)
                            return Token;

   --  Is_Delimited: some tokens are delimited by characters, or
   --  sequences of characters, which are not truly part of the token
   --  itself.  String and character literals generally fall into
   --  this category.

   function Is_Delimited (Frame : Token_Frame;
                          Tok   : Token) return Boolean;

   --  Get_Class: get the class of the token.

   function Get_Class (Frame : Token_Frame;
                       Tok   : Token) return Token_Class;

   --  Get_Token_Class_Name: get the combined token and class name
   --  If the token is reserved, the prepends the text "reserved_"
   --  before the name of the token's class; otherwise it just
   --  returns the class name
   function Get_Token_Class_Name (Frame : Token_Frame;
                                  Tok   : Token)
                                 return String;

   function Get_Name (Frame : Token_Frame;
                      Class : Token_Class)
                     return String;

   function Get_Name (Frame : Token_Frame;
                      Tok   : Token)
                     return String;

   --  Create_Token_Class: create a new token class.  Also creates
   --  a Token that can be used to match against this class, which
   --  is retrieved via Get_Class_Token.
   --    Frame:     The frame that stores this class
   --    Name:      This will be used whenever the class is displayed
   --    Lex:       A Lexer that can identify this class
   --    Delimiter: True if this class is its own delimiter; i.e.
   --               whenever one of the characters from Start is
   --               encountered, it completes the token immediately
   --               If Delimiter is True, the value of Lex is ignored.

   procedure Create_Token_Class
     (Frame     : in out Token_Frame;
      Name      : String;
      Delimiter : Boolean;
      Lex       : Aquarius.Lexers.Lexer;
      Class     :    out Token_Class);

   --  Create_Reserved_Token: create a new reserved token, which
   --  will be placed in the appropriate class

   procedure Create_Reserved_Token
     (Frame     : in out Token_Frame;
      Text      : String;
      Tok       :    out Token);

   function Exists
     (Frame     : Token_Frame;
      Text      : String)
     return Boolean;

   function Get_Token
     (Frame     : Token_Frame;
      Text      : String)
     return Token;

   procedure Create_Source_Token
     (Frame     : in out Token_Frame;
      Text      : String;
      Tok       :    out Token);
   --  Create a token defined by the source text, such as a string,
   --  identifier, or number

   procedure Scan (Frame      : Token_Frame;
                   Text       : String;
                   Partial    : Boolean;
                   Complete   :    out Boolean;
                   Have_Class :    out Boolean;
                   Unique     :    out Boolean;
                   Class      :    out Token_Class;
                   Tok        :    out Token;
                   First      : in out Positive;
                   Last       :    out Natural;
                   Token_OK   : access
                     function (Tok : Token) return Boolean);
   --  Scan: scan from the beginning of Text, looking for the first
   --  token.  If a complete token is found, Complete is set to True
   --  and the class, token and the first and last index of the token
   --  text is returned.  Otherwise, Complete is set to False.  It is
   --  often possible to detect the token's class before the token
   --  itself is complete.  If this is the case, Have_Class is set to
   --  True, and the Class argument is filled in.
   --  Partial should be set to True if Text does not necessarily
   --  contain a full line, for example if it is being entered
   --  interactively.  If Partial is False, then the end of text
   --  will be treated as a delimiter.
   --  If Token_OK is not null, it will be called when there are
   --  multiple token matches to attempt disambiguation.

private

   type Token_Class is new Natural;
   type Token is range 0 .. 255;

   subtype Real_Token_Class is Token_Class range 1 .. Token_Class'Last;
   subtype Real_Token is Token range 1 .. Token'Last;

   type Token_Text_Length_Class is (Short, Long);

   Max_Short_Length : constant := 16;

   type Long_Token_Text is access constant String;

   type Token_Text (Length_Class : Token_Text_Length_Class := Short) is
      record
         case Length_Class is
            when Short =>
               Text      : String (1 .. Max_Short_Length) := [others => ' '];
            when Long =>
               Long_Text : Long_Token_Text;
         end case;
      end record;

   Null_Token_Class : constant Token_Class := 0;

   type Token_Class_Info is
      record
         Id          : Token_Class;
         Parent      : Token_Class;
         Name        : Token_Text;
         Delimiter   : Boolean;
         Lex         : Aquarius.Lexers.Lexer;
         Start       : Ada.Strings.Maps.Character_Set;
         Class_Token : Token;
      end record;

   type Token_Info is
      record
         Id            : Token;
         Class         : Token_Class;
         Reserved      : Boolean;
         Text          : Token_Text;
         Standard_Text : Token_Text;
      end record;

   package Token_Class_Vectors is
      new Ada.Containers.Vectors (Real_Token_Class, Token_Class_Info);

   package Token_Vectors is
      new Ada.Containers.Vectors (Real_Token, Token_Info);

   type Token_Frame_Record is
      record
         Case_Sensitive : Boolean;
         Class_Vector   : Token_Class_Vectors.Vector;
         Token_Vector   : Token_Vectors.Vector;
      end record;

   type Token_Frame is access Token_Frame_Record;

--     package Set_Of_Tokens is
--        new Ada.Containers.Ordered_Sets (Real_Token);
--     type Token_Set is new Set_Of_Tokens.Set with null record;

   type Token_Set is array (Token) of Boolean;
   pragma Pack (Token_Set);

   Null_Token : constant Token := 0;

end Aquarius.Tokens;
