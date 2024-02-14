with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

package body Aquarius.Tokens is

   function To_Standard_Token_Text (S              : String;
                                    Case_Sensitive : Boolean)
                                   return Token_Text;

   function To_Standard_Token_Text (S              : String;
                                    Case_Sensitive : Boolean)
                                   return String;

   --  Returns the corresponding reserved token, or, if none
   --  can be found, returns the class token of Class.

   procedure Get_Reserved_Token (Frame  : Token_Frame;
                                 Class  : Token_Class;
                                 Text   : String;
                                 Result :    out Token);

   function Find_Class
     (Frame          : Token_Frame;
      Text           : String;
      Report_Missing : Boolean := True;
      Full_Text      : Boolean := False)
      return Token_Class;
   --  Find the class which matches the text.  If Report_Missing is True,
   --  then failing to find a class will cause a complaint to be written
   --  to Standard_Error.  If Full_Text is True, only a class which matches
   --  every character in the text will be considered

   type Array_Of_Token_Classes is array (Positive range <>) of Token_Class;

   function Find_Classes
     (Frame          : Token_Frame;
      Text           : String)
      return Array_Of_Token_Classes;
   --  Find all token classes which match Text, starting at Text'First,
   --  and matching any positive number of characters.

   function Find_Class_By_Name
     (Frame : Token_Frame;
      Name  : String)
      return Token_Class;

   ----------
   -- "<=" --
   ----------

   function "<=" (Left   : Token;
                  Right  : Token_Set)
                 return Boolean
   is
   begin
      --  return Right.Contains (Left);
      return Right (Left);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Left  : Token_Text;
                 Right : String)
                return Boolean
   is
   begin
      return To_String (Left) = Right;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left  : String;
                 Right : Token_Text)
                return Boolean
   is
   begin
      return Left = To_String (Right);
   end "=";

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (Left, Right : Token_Text) return Boolean is
   begin
      return To_String (Left) = To_String (Right);
   end "=";

   ---------
   -- Add --
   ---------

   procedure Add (Tok    :        Token;
                  To_Set : in out Token_Set)
   is
   begin
      To_Set (Tok) := True;
      --  To_Set.Include (Tok);
   end Add;

   ---------------------------
   -- Create_Reserved_Token --
   ---------------------------

   procedure Create_Reserved_Token
     (Frame     : in out Token_Frame;
      Text      : String;
      Tok       :    out Token)
   is
      Class    : Token_Class := Find_Class (Frame, Text,
                                            Report_Missing => False,
                                            Full_Text      => True);
      Tok_Info : Token_Info :=
                   (Id            => Frame.Token_Vector.Last_Index + 1,
                    Class         => Class,
                    Reserved      => True,
                    Text          => To_Token_Text (Text),
                    Standard_Text =>
                      To_Standard_Token_Text
                        (Text,
                         Frame.Case_Sensitive));
   begin
      if Class = Null_Token_Class then
         declare
            use Aquarius.Lexers;
            Lex : constant Lexer := Sequence (Text);
         begin
            Class := Find_Class_By_Name (Frame, "$symbol");
            if Class = Null_Token_Class then
               Create_Token_Class
                 (Frame     => Frame,
                  Name      => "$symbol",
                  Delimiter => False,
                  Lex       => Lex,
                  Class     => Class);
            else
               Frame.Class_Vector (Class).Lex :=
                 Frame.Class_Vector (Class).Lex or Lex;
            end if;
            Tok_Info.Class := Class;
         end;
      end if;
      Frame.Token_Vector.Append (Tok_Info);
      Tok := Tok_Info.Id;
   end Create_Reserved_Token;

   -------------------------
   -- Create_Source_Token --
   -------------------------

   procedure Create_Source_Token
     (Frame     : in out Token_Frame;
      Text      : String;
      Tok       :    out Token)
   is
      Class : constant Token_Class := Find_Class (Frame, Text);
      Tok_Info : constant Token_Info :=
        (Id            => Frame.Token_Vector.Last_Index + 1,
         Class         => Class,
         Reserved      => False,
         Text          => To_Token_Text (Text),
         Standard_Text => To_Standard_Token_Text (Text,
                                                  Frame.Case_Sensitive));
   begin
      Frame.Token_Vector.Append (Tok_Info);
      Tok := Tok_Info.Id;
   end Create_Source_Token;

   ------------------------
   -- Create_Token_Class --
   ------------------------

   procedure Create_Token_Class
     (Frame     : in out Token_Frame;
      Name      : String;
      Delimiter : Boolean;
      Lex       : Aquarius.Lexers.Lexer;
      Class     :    out Token_Class)
   is
      Class_Info : constant Token_Class_Info :=
        (Id          => Frame.Class_Vector.Last_Index + 1,
         Parent      => Null_Token_Class,
         Name        => To_Token_Text (Name),
         Delimiter   => Delimiter,
         Lex         => Lex,
         Start       => Aquarius.Lexers.Start (Lex),
         Class_Token => Frame.Token_Vector.Last_Index + 1);
      Tok_Info   : constant Token_Info :=
        (Id            => Frame.Token_Vector.Last_Index + 1,
         Class         => Class_Info.Id,
         Reserved      => False,
         Text          => To_Token_Text (Name),
         Standard_Text => To_Standard_Token_Text (Name,
           Frame.Case_Sensitive));
   begin
      Frame.Class_Vector.Append (Class_Info);
      Frame.Token_Vector.Append (Tok_Info);
      Class := Class_Info.Id;
   end Create_Token_Class;

   -----------
   -- Empty --
   -----------

   function Empty return Token_Set
   is
      Result : constant Token_Set := [others => False];
   begin
      return Result;
      --  Token_Set'(Set_Of_Tokens.Empty_Set with null record);
   end Empty;

   ------------
   -- Exists --
   ------------

   function Exists
     (Frame     : Token_Frame;
      Text      : String)
     return Boolean
   is
   begin
      for I in 1 .. Frame.Token_Vector.Last_Index loop
         declare
            Tok : Token_Info renames Frame.Token_Vector.Element (I);
         begin
            if Tok.Text = Text then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Exists;

   ----------------
   -- Find_Class --
   ----------------

   function Find_Class
     (Frame          : Token_Frame;
      Text           : String;
      Report_Missing : Boolean := True;
      Full_Text      : Boolean := False)
      return Token_Class
   is
      Possible       : Token_Class;
      Unique         : Boolean      := False;
      Possible_Count : Natural      := 0;
      Best_Match     : Natural      := 0;
      Classes        : array (Token_Class
                              range 0 .. Frame.Class_Vector.Last_Index)
        of Boolean := [others => False];

   begin
      Possible_Count := 0;
      for I in 1 .. Frame.Class_Vector.Last_Index loop
         declare
            Class : Token_Class_Info renames
              Frame.Class_Vector.Element (I);
            Length : constant Natural :=
              Aquarius.Lexers.Run (Class.Lex, Text);
         begin

            if Length > Text'Last
              or else (not Full_Text and then Length > Text'First)
            then
               if Possible_Count = 0 or else Length > Best_Match then
                  Unique := True;
                  Possible_Count := 1;
                  Best_Match := Length;
                  Possible := I;
                  Classes := [others => False];
               elsif Length = Best_Match then
                  Possible_Count := Possible_Count + 1;
                  Unique := False;
               end if;
               Classes (I) := True;
            end if;
         end;
      end loop;
      if Possible_Count = 0 then
         if Report_Missing then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "no class found for text: " & Text);
         end if;
      elsif not Unique then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "multiple matching classes found for text: " & Text);
         for Class in Classes'Range loop
            if Classes (Class) then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "   matching class: " & Get_Name (Frame, Class));
            end if;
         end loop;
         raise Constraint_Error with
           "multiple matching classes found for text: " & Text;
      end if;

      if Unique then
         return Possible;
      else
         return Null_Token_Class;
      end if;
   end Find_Class;

   ------------------------
   -- Find_Class_By_Name --
   ------------------------

   function Find_Class_By_Name
     (Frame : Token_Frame;
      Name  : String)
      return Token_Class
   is
   begin
      for Result in
        Real_Token_Class'First .. Frame.Class_Vector.Last_Index
      loop
         if Frame.Class_Vector (Result).Name = Name then
            return Result;
         end if;
      end loop;
      return Null_Token_Class;
   end Find_Class_By_Name;

   ------------------
   -- Find_Classes --
   ------------------

   function Find_Classes
     (Frame          : Token_Frame;
      Text           : String)
      return Array_Of_Token_Classes
   is
      Max    : constant Natural := Natural (Frame.Class_Vector.Length);
      Count  : Natural := 0;
      Result : Array_Of_Token_Classes (1 .. Max);
   begin
      for I in 1 .. Frame.Class_Vector.Last_Index loop
         declare
            Class  : Token_Class_Info renames Frame.Class_Vector (I);
            Length : constant Natural :=
                       Aquarius.Lexers.Run (Class.Lex, Text);
         begin
            if Length > 0 then
               Count := Count + 1;
               Result (Count) := I;
            end if;
         end;
      end loop;

      return Result (1 .. Count);
   end Find_Classes;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Frame : Token_Frame;
                       Tok   : Token)
                      return Token_Class
   is
   begin
      return Frame.Token_Vector.Element (Tok).Class;
   end Get_Class;

   ---------------------
   -- Get_Class_Token --
   ---------------------

   function Get_Class_Token (Frame : Token_Frame;
                             Class : Token_Class)
                            return Token
   is
   begin
      return Frame.Class_Vector.Element (Class).Class_Token;
   end Get_Class_Token;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Frame : Token_Frame;
                      Class : Token_Class)
                     return String
   is
   begin
      return To_String (Frame.Class_Vector.Element (Class).Name);
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Frame : Token_Frame;
                      Tok   : Token)
                     return String
   is
   begin
      if Tok in 1 .. Frame.Token_Vector.Last_Index then
         return To_String (Frame.Token_Vector.Element (Tok).Text);
      elsif Tok = 0 then
         raise Constraint_Error with
           "attempt to get name of null token";
      else
         raise Constraint_Error with
           "token" & Tok'Img & " does not exist";
      end if;
   end Get_Name;

   ------------------------
   -- Get_Reserved_Token --
   ------------------------

   procedure Get_Reserved_Token (Frame  : Token_Frame;
                                 Class  : Token_Class;
                                 Text   : String;
                                 Result :    out Token)
   is
      Std_Text : constant String :=
        To_Standard_Token_Text (Text, Frame.Case_Sensitive);
   begin
      for I in 1 .. Frame.Token_Vector.Last_Index loop
         declare
            Tok : Token_Info renames Frame.Token_Vector.Element (I);
         begin
            if Tok.Class = Class and then Tok.Text = Std_Text then
               Result := Tok.Id;
               return;
            end if;
         end;
      end loop;
      Result := Frame.Class_Vector.Element (Class).Class_Token;
   end Get_Reserved_Token;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token
     (Frame     : Token_Frame;
      Text      : String)
     return Token
   is
   begin
      for I in 1 .. Frame.Token_Vector.Last_Index loop
         declare
            Tok : Token_Info renames Frame.Token_Vector.Element (I);
         begin
            if Tok.Text = Text then
               return Tok.Id;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        "no such token: " & Text;
   end Get_Token;

   --------------------------
   -- Get_Token_Class_Name --
   --------------------------

   function Get_Token_Class_Name (Frame : Token_Frame;
                                  Tok   : Token)
                                 return String
   is
      Class    : constant Token_Class := Get_Class (Frame, Tok);
      Result   : constant String      := Get_Name (Frame, Class);
   begin
      if Frame.Token_Vector.Element (Tok).Reserved then
         return "reserved_" & Result;
      else
         return Result;
      end if;
   end Get_Token_Class_Name;

   --------------------
   -- Is_Class_Token --
   --------------------

   function Is_Class_Token (Frame : Token_Frame;
                            Tok   : Token) return Boolean
   is
   begin
      return not Is_Reserved (Frame, Tok);
   end Is_Class_Token;

   ------------------
   -- Is_Delimited --
   ------------------

   function Is_Delimited (Frame : Token_Frame;
                          Tok   : Token)
                         return Boolean
   is
      --  we haven't implemented this yet
      pragma Unreferenced (Frame);
      pragma Unreferenced (Tok);
   begin
      return False;
   end Is_Delimited;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Frame : Token_Frame;
                         Tok   : Token)
                        return Boolean
   is
   begin
      return Frame.Token_Vector.Element (Tok).Reserved;
   end Is_Reserved;

   ---------------
   -- New_Frame --
   ---------------

   function New_Frame
     (Case_Sensitive : Boolean)
     return Token_Frame
   is
      Frame : constant Token_Frame := new Token_Frame_Record;
   begin
      Frame.Case_Sensitive := Case_Sensitive;
      Frame.Class_Vector.Clear;
      Frame.Token_Vector.Clear;
      return Frame;
   end New_Frame;

   ----------
   -- Scan --
   ----------

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
                     function (Tok : Token) return Boolean)
   is
   begin
      Unique := True;
      Tok := Null_Token;
      Class := Null_Token_Class;
      Have_Class := False;
      Last := 0;

      while First <= Text'Last and then Text (First) = ' ' loop
         First := First + 1;
      end loop;

      if First > Text'Last then
         Complete   := False;
         Have_Class := False;
         Last       := First;
         return;
      end if;

      declare
         Classes : constant Array_Of_Token_Classes :=
                     Find_Classes (Frame, Text (First .. Text'Last));
      begin

         if Classes'Length = 0 then
            Have_Class := False;
            Complete   := True;
         else
            for Test_Class of Classes loop
               declare
                  Class_Info : Token_Class_Info renames
                                 Frame.Class_Vector.Element (Test_Class);
                  Temp_Last  : Natural;
               begin
                  Temp_Last :=
                    Aquarius.Lexers.Run
                      (Class_Info.Lex,
                       Text (First .. Text'Last)) - 1;

                  if Temp_Last >= First and then
                    (Temp_Last < Text'Last
                     or else (Temp_Last >= Text'Last and then not Partial)
                     or else Class_Info.Delimiter)
                  then
                     if Have_Class then
                        Have_Class := Token_OK = null or else Token_OK (Tok);
                     end if;

                     if Temp_Last > Text'Last then
                        Temp_Last := Text'Last;
                     end if;

                     declare
                        Test_Tok : Token;
                     begin
                        Get_Reserved_Token (Frame, Test_Class,
                                            Text (First .. Temp_Last),
                                            Test_Tok);

                        if Have_Class then
                           if Token_OK = null or else Token_OK (Test_Tok) then
                              Unique := False;
                           end if;
                        else
                           Class := Test_Class;
                           Tok   := Test_Tok;
                           Last  := Temp_Last;
                           Complete := not Partial
                             or else Last < Text'Last
                             or else Class_Info.Delimiter;
                        end if;
                     end;
                     Have_Class := True;

                  end if;
               end;
            end loop;
         end if;
      end;

   end Scan;

   ------------------------
   -- Set_Case_Sensitive --
   ------------------------

   procedure Set_Case_Sensitive (Frame : Token_Frame;
                                 Value : Boolean)
   is
   begin
      Frame.Case_Sensitive := Value;
   end Set_Case_Sensitive;

   ---------------
   -- Terminals --
   ---------------

   function Terminals (Frame : Token_Frame) return Array_Of_Tokens is
      Result : Array_Of_Tokens (1 .. Natural (Frame.Token_Vector.Last_Index));
   begin
      for I in Result'Range loop
         Result (I) := Token (I);
      end loop;
      return Result;
   end Terminals;

   ----------------------------
   -- To_Standard_Token_Text --
   ----------------------------

   function To_Standard_Token_Text (S              : String;
                                    Case_Sensitive : Boolean)
                                   return Token_Text
   is
      use Ada.Strings.Fixed, Ada.Strings.Maps.Constants;
   begin
      if Case_Sensitive then
         return To_Token_Text (S);
      else
         return To_Token_Text (Translate (S, Lower_Case_Map));
      end if;
   end To_Standard_Token_Text;

   ----------------------------
   -- To_Standard_Token_Text --
   ----------------------------

   function To_Standard_Token_Text (S              : String;
                                    Case_Sensitive : Boolean)
                                   return String
   is
   begin
      if Case_Sensitive then
         return S;
      else
         return Ada.Characters.Handling.To_Lower (S);
      end if;
   end To_Standard_Token_Text;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Token_Text) return String is
   begin
      case Item.Length_Class is
         when Short =>
            return Ada.Strings.Fixed.Trim (Item.Text, Ada.Strings.Right);
         when Long =>
            return Item.Long_Text.all;
      end case;
   end To_String;

   -------------------
   -- To_Token_Text --
   -------------------

   function To_Token_Text (Item : String) return Token_Text is
   begin
      if Item'Length > Max_Short_Length then
         return (Long, new String'(Item));
      else
         declare
            Text : String (1 .. Max_Short_Length) := (others => ' ');
         begin
            Text (1 .. Item'Length) := Item;
            return (Short, Text);
         end;
      end if;
   end To_Token_Text;

end Aquarius.Tokens;
