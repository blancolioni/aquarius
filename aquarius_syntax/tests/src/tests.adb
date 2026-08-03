with Ada.Command_Line;
with Ada.Text_IO;

with Aquarius.Docs;
with Fake_Terminals;

procedure Tests is

   use Aquarius.Docs;
   use Fake_Terminals;

   Failures : Natural := 0;

   -----------
   -- Check --
   -----------

   procedure Check (Name : String; Passed : Boolean) is
   begin
      if Passed then
         Ada.Text_IO.Put_Line ("pass: " & Name);
      else
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   -----------------
   -- Leaf_Alone --
   -----------------

   procedure Leaf_Alone is
      T : constant Fake_Terminal_Access := Make ("hello");
   begin
      Layout (Leaf (T), Width => 80, Start_Line => 1, Start_Column => 1);
      Check ("leaf alone: line", Line (T.all) = 1);
      Check ("leaf alone: column", Column (T.all) = 1);
   end Leaf_Alone;

   --------------------
   -- Concat_Leaves --
   --------------------

   procedure Concat_Leaves is
      Foo : constant Fake_Terminal_Access := Make ("foo");
      Bar : constant Fake_Terminal_Access := Make ("bar");
   begin
      Layout
        (Leaf (Foo) & Leaf (Bar),
         Width => 80, Start_Line => 1, Start_Column => 1);
      Check ("concat: first leaf at column 1", Column (Foo.all) = 1);
      Check ("concat: second leaf column advances by first's length",
             Column (Bar.all) = 4);
      Check ("concat: both on line 1",
             Line (Foo.all) = 1 and then Line (Bar.all) = 1);
   end Concat_Leaves;

   -------------------
   -- Group_Fits --
   -------------------

   procedure Group_Fits is
      A : constant Fake_Terminal_Access := Make ("a");
      B : constant Fake_Terminal_Access := Make ("b");
   begin
      Layout
        (Group (Leaf (A) & Line & Leaf (B)),
         Width => 80, Start_Line => 1, Start_Column => 1);
      Check ("group fits: stays on one line",
             Line (A.all) = 1 and then Line (B.all) = 1);
      Check ("group fits: line renders as a space",
             Column (A.all) = 1 and then Column (B.all) = 3);
   end Group_Fits;

   ----------------------
   -- Group_Overflows --
   ----------------------

   procedure Group_Overflows is
      A : constant Fake_Terminal_Access := Make ("aaaaaaaaaa");
      B : constant Fake_Terminal_Access := Make ("bbbbbbbbbb");
   begin
      Layout
        (Group (Leaf (A) & Line & Leaf (B)),
         Width => 5, Start_Line => 1, Start_Column => 1);
      Check ("group overflows: first leaf on line 1", Line (A.all) = 1);
      Check ("group overflows: second leaf pushed to line 2",
             Line (B.all) = 2);
      Check ("group overflows: column resets to the group's indent",
             Column (B.all) = 1);
      Check ("group overflows: first leaf offset starts at 0",
             Offset (A.all) = 0);
      Check ("group overflows: second leaf offset accounts for the " &
             "newline and indent (10 chars + 1 newline + 0 indent)",
             Offset (B.all) = 11);
   end Group_Overflows;

   ------------------------------
   -- Group_Breaks_For_Tail --
   ------------------------------

   procedure Group_Breaks_For_Tail is
      --  The Lindig case: the group's OWN flattened content ("short
      --  txt", 9 chars) fits within Width (10) taken alone. A naive
      --  per-group check that ignores what follows on the same line
      --  would keep this flat. But it is immediately followed (same
      --  Concat, no intervening break) by a 15-character leaf that
      --  pushes the total to 24, so the correct decision is to break.
      Short : constant Fake_Terminal_Access := Make ("short");
      Txt   : constant Fake_Terminal_Access := Make ("txt");
      Extra : constant Fake_Terminal_Access := Make ("EXTRA_LONG_TAIL");
   begin
      Layout
        (Group (Leaf (Short) & Line & Leaf (Txt)) & Leaf (Extra),
         Width => 10, Start_Line => 1, Start_Column => 1);
      Check ("continuation pressure: group breaks despite fitting alone",
             Line (Txt.all) = 2);
      Check ("continuation pressure: first leaf unaffected",
             Line (Short.all) = 1 and then Column (Short.all) = 1);
      Check ("continuation pressure: broken leaf at indent",
             Column (Txt.all) = 1);
      Check ("continuation pressure: tail continues right after",
             Line (Extra.all) = 2 and then Column (Extra.all) = 4);
   end Group_Breaks_For_Tail;

   --------------------------
   -- Nested_Group_In_Nest --
   --------------------------

   procedure Nested_Group_In_Nest is
      --  Outer group cannot fit flat (16 chars of content against a
      --  width of 8) and breaks; the inner group, checked
      --  independently once the outer has already broken and reset
      --  the column via Nest's offset, still fits flat on its own.
      Head : constant Fake_Terminal_Access := Make ("HEADHEADHEAD");
      X    : constant Fake_Terminal_Access := Make ("x");
      Y    : constant Fake_Terminal_Access := Make ("y");
   begin
      Layout
        (Group
           (Leaf (Head)
            & Nest (3, Line & Group (Leaf (X) & Line & Leaf (Y)))),
         Width => 8, Start_Line => 1, Start_Column => 1);
      Check ("nested group: outer breaks", Line (X.all) = 2);
      Check ("nested group: broken line indented by Nest's offset",
             Column (X.all) = 4);
      Check ("nested group: inner group still fits flat",
             Line (Y.all) = 2 and then Column (Y.all) = 6);
   end Nested_Group_In_Nest;

begin
   Leaf_Alone;
   Concat_Leaves;
   Group_Fits;
   Group_Overflows;
   Group_Breaks_For_Tail;
   Nested_Group_In_Nest;

   Ada.Text_IO.Put_Line ("failures:" & Failures'Image);
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Tests;
