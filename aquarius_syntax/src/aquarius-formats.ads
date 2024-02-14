package Aquarius.Formats is

   --  pragma Preelaborate;

   --  Aquarius_Format: describes how to format a program tree node
   --  for output.  Describes the whitespace surrounding it (spaces,
   --  new lines, "soft" new lines, indentation, font properties)
   type Aquarius_Format is private;

   --  A single rule within a format.  Describes a single element
   --  of the format (e.g. "should we have a space before this node?").
   type Format_Rule is private;

   --  Rule_Priority: the "strength" of a format rule.  0 is very low
   --  priority, 9 is very high.  If two format rules conflict, the
   --  highest priority is chosen.  If priorities are equal, one of
   --  them is chosen arbitrarily (well, not arbitrarily, but the choice
   --  should not be relied upon).
   type Rule_Priority is range 0 .. 9;

   --  Indentation_Offset changes the current indendation by
   --  the given amount.  It's not expected that we use the
   --  full range of the integer type, but it's hard to come
   --  up with an obviously correct limit.

   type Indentation_Offset is new Integer;

   type Array_Of_Rules is array (Positive range <>) of Format_Rule;
   function Make_Format (Rule : Format_Rule) return Aquarius_Format;
   function Make_Format (Rules : Array_Of_Rules) return Aquarius_Format;
   procedure Add_Rule (Format : in out Aquarius_Format;
                       Rule   : Format_Rule);

   --  Handy space format functions.
   --  Space_Always puts a priority 7 space before or after the node
   --  Space_Never suppresses spaces up to priority 6
   --  Space_Sometimes puts a priority 5 space before or after the node
   function Space_Always    (Position : Rule_Position) return Format_Rule;
   function Space_Never     (Position : Rule_Position) return Format_Rule;
   function Space_Sometimes (Position : Rule_Position) return Format_Rule;

   --  General space function.  If Add_Space is True, the rule
   --  tries to add a space character; otherwise, it is for not
   --  having a space.
   function Space (Add_Space  : Boolean;
                   Position   : Rule_Position;
                   Priority   : Rule_Priority)
                  return Format_Rule;

   --  Examples of space formats:
   --     An identifier with priority 7 spaces before and after.
   --     A dot ('.') with priority 9 no-space before and after.
   --     Open parenthesis with priority 8 space before and priority 9
   --     no-space after.
   --     Close parenthesis with priority 9 no-space before and after.
   --  With this set of formats, we see that most identifiers are
   --  separated by spaces (note that reserved words are a subclass of
   --  identifiers, so they are covered by this rule as well).
   --  Identifiers separated by dots have this overridden, leading
   --  to the sequence 'Ada' '.'  'Text_IO' being formatted as
   --  Ada.Text_IO instead of Ada . Text_IO
   --  There is almost always a space before an open parenthesis,
   --  unless it is overridden, which another open parenthesis will
   --  do with its space-after rule; thus
   --      Ada.Text_IO.Put_Line ("Hello, world")
   --  but
   --      Ada.Text_IO.Put_Line (("Hello" & ",") & " world");

   --  Formatting of new lines
   --  A node can require a new line before or after it, possibly in
   --  conjunction with indentation changes.
   --  A new line can be "soft"; this means that the format is not
   --  applied unless the line is too long to comfortably fit on
   --  the screen; this is set dynamically.
   --  A node with New_Line after followed by a node with New_Line
   --  before results in only one new line.

   --  Handy new line formats

   --  New_Line: puts a priority 5 new line before or after the node
   function New_Line (Position : Rule_Position) return Format_Rule;

   --  Soft_New_Line: same as New_Line, except it is only applied
   --  when necessary
   function Soft_New_Line (Position : Rule_Position) return Format_Rule;

   --  Change the indent before or after the node.
   --  TODO: should a new line be automatic in this case?
   function Indent (Position : Rule_Position;
                    Offset   : Indentation_Offset)
                   return Format_Rule;

   --  It's often useful to require children to be indented.  This
   --  rule takes effect on all direct or indirect child nodes of
   --  the current rule that are the first in their line to be
   --  rendered.  The original indent is automatically restored before
   --  the beginning of the next node; so in the sense of structured
   --  formatting, this rule is preferred over Indent.

   function Indent_Child (Offset : Indentation_Offset)
                         return Format_Rule;

   function Opening return Format_Rule;
   function Closing return Format_Rule;
   --  Opening and closing terminals are those which bracket another piece
   --  of syntax.  For example, in Ada 'if' and 'then' bracket an expression.
   --  When the opening and closing termals start on the same line, but
   --  after rendering they end up on different lines, the closing terminal
   --  should be moved to its own line, in the same column as the opening.

   --  If this is too complicated (which it is), let's think of a more
   --  basic way of getting this effect.

   --  Example of indentation formats

   --  given the following syntax:
   --     procedure ::= 'procedure' identifier 'is' block
   --     block ::= sequence-of-declarations
   --               'begin' sequence-of-statements
   --               'end' identifier ';'

   --  the children of block called 'sequence-of-declarations' and
   --  'sequence-of-statements' should both be indented by three,
   --  using the Indent_Child rule.  We don't need to explicitly
   --  use New_Line rules on 'is', 'begin' and 'end', because
   --  the Indent_Child does that for us.  (TODO: is this true?)

   --  Handy default formats
   --  Default_Terminal_Format: priority 7 spaces before and after
   function Default_Terminal_Format return Aquarius_Format;
   function Default_Non_Terminal_Format return Aquarius_Format;

   --  Join: merge two formats into one.  In the case of conflicts,
   --  the rules from Right supercede those from Left
   function Join (Left, Right : Aquarius_Format)
                 return Aquarius_Format;

   type Immediate_Rules is
      record
         New_Line_Before, New_Line_After : Format_Rule;
         Space_Before, Space_After       : Format_Rule;
         Soft_New_Line_Before            : Format_Rule;
         Soft_New_Line_After             : Format_Rule;
         Opening, Closing                : Format_Rule;
      end record;

   function Rules (Format : Aquarius_Format) return Immediate_Rules;

   function Priority (Rule : Format_Rule) return Rule_Priority;
   function Enabled  (Rule : Format_Rule) return Boolean;
   function Negative (Rule : Format_Rule) return Boolean;

   function Indent (Format   : Aquarius_Format;
                    Position : Rule_Position)
                   return Indentation_Offset;

   function Indent_Child (Format : Aquarius_Format)
                         return Indentation_Offset;

   function Image (Item : Aquarius_Format) return String;

   --  turn a named rule into a rule
   function To_Rule (Text : String) return Format_Rule;

   --  To_Format: turn a named format into a format.  This can
   --  be the name of a predefined format, or a single rule
   --  which is handled by To_Rule
   function To_Format (Text : String) return Aquarius_Format;

private

   type Format_Rule_Type is (No_Rule, Space_Rule, New_Line_Rule,
                             Indent_Rule, Indent_Child_Rule,
                             Bracketing_Rule);

   type Format_Rule (Rule_Type : Format_Rule_Type := No_Rule) is
      record
         Position : Rule_Position;
         Priority : Rule_Priority;
         case Rule_Type is
            when No_Rule =>
               null;
            when Space_Rule =>
               Add_Space : Boolean;
            when New_Line_Rule =>
               Soft : Boolean;
            when Indent_Rule | Indent_Child_Rule =>
               Offset    : Indentation_Offset;
            when Bracketing_Rule =>
               null;
         end case;
      end record;

   function Equivalent (Left, Right : Format_Rule) return Boolean;

   Max_Rules : constant := 10;

   subtype Rule_Count is Natural range 0 .. Max_Rules;
   subtype Rule_Index is Rule_Count range 1 .. Rule_Count'Last;

   type Aquarius_Format is
      record
         Num_Rules : Rule_Count := 0;
         Rules     : Array_Of_Rules (Rule_Index);
      end record;

end Aquarius.Formats;
