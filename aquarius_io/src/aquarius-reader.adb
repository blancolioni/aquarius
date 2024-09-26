with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Errors;
with Aquarius.Interaction.Console;
with Aquarius.Locations;
with Aquarius.Programs.Parser;
with Aquarius.Properties;
with Aquarius.Syntax;
with Aquarius.Tokens;
with Aquarius.Workspaces;

package body Aquarius.Reader is

   procedure Get_Line
     (Stream         : Aquarius.Streams.Reader_Reference;
      Line           : out Ada.Strings.Unbounded.Unbounded_String;
      Line_Start     : in out Locations.Updateable_Location_Interface'Class;
      Line_Number    : in out Positive;
      Grammar        : Aquarius.Grammars.Aquarius_Grammar;
      Vertical_Space : out Aquarius.Locations.Line_Count);

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Stream         : Aquarius.Streams.Reader_Reference;
      Line           : out Ada.Strings.Unbounded.Unbounded_String;
      Line_Start     : in out Locations.Updateable_Location_Interface'Class;
      Line_Number    : in out Positive;
      Grammar        : Aquarius.Grammars.Aquarius_Grammar;
      Vertical_Space : out Aquarius.Locations.Line_Count)
   is
      use Ada.Strings.Unbounded;
      use type Aquarius.Locations.Line_Count;

      function Expand_Tabs (S : String) return String;
      function Strip (S : String) return String;
      function Trim (S : String) return String;

      -----------------
      -- Expand_Tabs --
      -----------------

      function Expand_Tabs (S : String) return String is
         Count : Natural := 0;
      begin
         for I in S'Range loop
            declare
               Ch : constant Character := S (I);
            begin
               if Ch = Character'Val (9) then
                  declare
                     Space_Count : constant Positive :=
                                     8 - Count mod 8;
                     Spaces      : constant String (1 .. Space_Count) :=
                                     [others => ' '];
                  begin
                     return S (S'First .. I - 1)
                       & Spaces
                       & Expand_Tabs (S (I + 1 .. S'Last));
                  end;
               end if;
               Count := Count + 1;
            end;
         end loop;
         return S;
      end Expand_Tabs;

      -----------
      -- Strip --
      -----------

      function Strip (S : String) return String is
         Last : Natural := S'Last;
      begin
         while Last > 0 and then S (Last) = Character'Val (13) loop
            Last := Last - 1;
         end loop;
         return S (S'First .. Last);
      end Strip;

      ----------
      -- Trim --
      ----------

      function Trim (S : String) return String is
         Left : Positive := S'First;
         Right : Natural := S'Last;
         function Is_Space (Ch : Character) return Boolean
         is (not Ada.Characters.Handling.Is_Graphic (Ch));

      begin
         while Left <= S'Last and then Is_Space (S (Left)) loop
            Left := Left + 1;
         end loop;
         while Right >= S'First and then Is_Space (S (Right)) loop
            Right := Right - 1;
         end loop;
         return S (Left .. Right);
      end Trim;

   begin

      Vertical_Space := 0;

      while not Stream.End_Of_Stream loop
         Line_Start.Update_Location (Stream.all);
         declare
            Full_Line     : constant String := Strip (Stream.Get_Line);
            Expanded_Line : constant String := Expand_Tabs (Full_Line);
            Trimmed_Line  : constant String := Trim (Full_Line);
         begin
            if Trimmed_Line'Length > 0 then
               if Grammar.Significant_End_Of_Line then
                  Line := To_Unbounded_String (Expanded_Line)
                    & Character'Val (10);
               else
                  Line := To_Unbounded_String (Expanded_Line);
               end if;
               return;
            end if;
            Vertical_Space := Vertical_Space + 1;
            Line_Number := Line_Number + 1;
         end;
      end loop;

      Line := Null_Unbounded_String;
   end Get_Line;

   ----------
   -- Read --
   ----------

   function Read
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Store      : not null access Programs.Root_Program_Tree_Store'Class;
      Interactor : Aquarius.Interaction.Interactor_Access;
      Source     : Aquarius.Sources.Source_Reference;
      Stream     : Aquarius.Streams.Reader_Reference)
      return Aquarius.Programs.Program_Tree
   is
      use Aquarius.Programs, Aquarius.Programs.Parser;
      Top_Level : constant not null Aquarius.Syntax.Syntax_Tree :=
                    Grammar.Get_Top_Level_Syntax;
      Result    : constant Aquarius.Programs.Program_Tree :=
                    Aquarius.Programs.New_Program_Root
                      (Top_Level, Source, Stream.all, Store);
      Context    : Parse_Context;
      Recovering : Boolean := False;
      Line_Comment : constant String :=
                       (if Grammar.Have_Line_Comment
                        then Grammar.Line_Comment
                        else "");
      Block_Comment_Start : constant String :=
                              (if Grammar.Have_Block_Comment
                               then Grammar.Block_Comment_Start
                               else "");
      Block_Comment_End : constant String :=
                              (if Grammar.Have_Block_Comment
                               then Grammar.Block_Comment_End
                               else "");

      Line_Number       : Positive := 1;

      function Token_OK (Tok : Aquarius.Tokens.Token) return Boolean
      is (Token_OK (Tok, Context));

   begin

      Result.Set_Property
        (Prop => Aquarius.Properties.Grammar_Property, Value => Grammar);

      Result.Set_Property
        (Prop => Aquarius.Properties.Interactor_Property, Value => Interactor);

      Initialise_Parse_Context (Context, Grammar, Result,
                                Interactive => False);

      while not Stream.End_Of_Stream loop
         declare
            use Ada.Strings.Unbounded;
            use type Aquarius.Tokens.Token;
            Line           : Unbounded_String;
            Next, First    : Natural;
            Line_Start     : Aquarius.Locations.Instance;
            Line_Last      : Positive;
            Old_First      : Natural;
            Class          : Aquarius.Tokens.Token_Class;
            Tok            : Aquarius.Tokens.Token;
            Complete       : Boolean;
            Unique         : Boolean;
            Have_Class     : Boolean;
            Have_Error     : Boolean;
            Vertical_Space : Aquarius.Locations.Line_Count := 0;
            LF : constant Character :=
                   Ada.Characters.Latin_1.LF;
         begin

            Get_Line
              (Stream         => Stream,
               Line           => Line,
               Line_Start     => Line_Start,
               Line_Number    => Line_Number,
               Grammar        => Grammar,
               Vertical_Space => Vertical_Space);

            exit when Length (Line) = 0;

            Aquarius.Programs.Parser.Set_Vertical_Space
              (Context, Vertical_Space);

            Next    := 1;
            First   := 1;
            Line_Last := Length (Line);

            << Restart_Space_Scan >>

            while First <= Line_Last loop
               Old_First := First;
               Have_Error := False;

               --  don't try to parse remaining spaces on the line
               while First <= Line_Last
                 and then
                   (Ada.Characters.Handling.Is_Space
                      (Element (Line, First))
                    and then (not Grammar.Significant_End_Of_Line
                              or else Element (Line, First) /= LF))
               loop
                  First := First + 1;
               end loop;

               exit when First > Line_Last;

               --  explicit check for line comment
               if Grammar.Have_Line_Comment
                 and then First + Line_Comment'Length
                   <= Length (Line) + 1
               then
                  if Slice (Line, First, First + Line_Comment'Length - 1)
                    = Line_Comment
                  then
                     --  FIXME: save the text!

                     --  If end of line is significant, we have to treat
                     --  line comments as end of line; otherwise we simply
                     --  exit this loop which will take us to the next line.
                     if Grammar.Significant_End_Of_Line then
                        First := Line_Last;
                     else
                        exit;
                     end if;
                  end if;
               end if;

               --  explicit check for block comment
               if Grammar.Have_Block_Comment
                 and then First + Block_Comment_Start'Length - 1 <= Line_Last
                 and then Slice (Line, First,
                                 First + Block_Comment_Start'Length - 1)
                 = Block_Comment_Start
               then
                  First := First + Block_Comment_Start'Length;

                  declare
                     Found        : Boolean := False;
                  begin

                     while not Found loop

                        declare
                           End_Index : constant Natural :=
                                         Index
                                           (Line, Block_Comment_End, First);
                        begin
                           if End_Index > 0 then
                              First := End_Index + Block_Comment_End'Length;
                              Found := True;
                              exit;
                           end if;

                           Get_Line
                             (Stream         => Stream,
                              Line           => Line,
                              Line_Start     => Line_Start,
                              Line_Number    => Line_Number,
                              Grammar        => Grammar,
                              Vertical_Space => Vertical_Space);
                           First := 1;
                        end;
                     end loop;

                     goto Restart_Space_Scan;

                  end;
               end if;

               Context.Update_Location (Line_Start);

               Aquarius.Tokens.Scan
                 (Frame      => Grammar.Frame,
                  Text       => Slice (Line, 1, Line_Last),
                  Partial    => False,
                  Complete   => Complete,
                  Have_Class => Have_Class,
                  Unique     => Unique,
                  Class      => Class,
                  Tok        => Tok,
                  First      => First,
                  Last       => Next,
                  Token_OK   => Token_OK'Access);

               if Have_Class then
                  Context.Update_Column (Locations.Column_Index (First));

                  if Token_OK (Tok) then
                     Recovering := False;
                     Parse_Token (Tok, Slice (Line, First, Next), Context);
                  else
                     if not Recovering then
                        Ada.Text_IO.Put
                          (Ada.Text_IO.Standard_Error,
                           Source.Short_Name
                           & ": line" & Line_Number'Image
                           & ": syntax error at "
                           & Slice (Line, First, Next));

                        declare
                           use Aquarius.Tokens;
                           All_Terminals : constant Array_Of_Tokens :=
                                             Terminals (Grammar.Frame);
                        begin
                           Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                                            " (expected");
                           for T of All_Terminals loop
                              if Token_OK (T, Context) then
                                 Ada.Text_IO.Put
                                   (Ada.Text_IO.Standard_Error,
                                    " " & Get_Name (Grammar.Frame, T));
                              end if;
                           end loop;
                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error, ")");
                        end;

                        if False then
                           Summarise_Context (Context);
                        end if;

                        Have_Error := True;
                        Recovering := True;
                     end if;
                  end if;

                  Aquarius.Programs.Parser.Set_Vertical_Space
                    (Context, 0);
                  Aquarius.Programs.Parser.Clear_Comments
                    (Context);

               else
                  Have_Error := True;
                  declare
                     Msg : constant String :=
                             Source.Short_Name
                             & ": line" & Line_Number'Image
                             & ": unable to determine class of token '"
                             & Slice (Line, Old_First, Line_Last)
                             & "'";
                  begin
                     Aquarius.Errors.Error (null, null, Msg);
                  end;

                  --  Next is set to zero by the token scanner
                  --  if we don't get a class back.
                  Next := First;
               end if;

               if Have_Error then
                  Add_Error (Context,
                             Grammar.Make_Error_Tree
                               (Context, Slice (Line, First, Next)));
               end if;

               First := Next + 1;

            end loop;
         end;
      end loop;

      Finish_Parse (Context);

      Stream.Close;

      return Result;

   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Store      : not null access Programs.Root_Program_Tree_Store'Class;
      Source     : Aquarius.Sources.Source_Reference;
      Stream     : Aquarius.Streams.Reader_Reference)
      return Aquarius.Programs.Program_Tree
   is
   begin
      return Read
        (Grammar    => Grammar,
         Store      => Store,
         Interactor => Interaction.Console.Console_Interactor,
         Source     => Source,
         Stream     => Stream);
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Source     : Aquarius.Sources.Source_Reference;
      Stream     : Aquarius.Streams.Reader_Reference)
      return Aquarius.Programs.Program_Tree
   is
   begin
      return Read
        (Grammar    => Grammar,
         Store      =>
           Aquarius.Workspaces.New_Empty_Workspace
             (Ada.Directories.Current_Directory),
         Interactor => Interaction.Console.Console_Interactor,
         Source     => Source,
         Stream     => Stream);
   end Read;

end Aquarius.Reader;
