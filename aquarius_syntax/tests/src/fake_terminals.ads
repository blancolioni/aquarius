with Ada.Strings.Unbounded;

with Aquarius.Docs;

package Fake_Terminals is

   --  A minimal Terminal_Node test double: fixed text, plus captured
   --  Offset/Line/Column that Set_Position writes into, so a test can
   --  build a Doc, call Layout, and assert the captured positions --
   --  no grammar, parser, or Program_Tree involved.

   type Fake_Terminal is new Aquarius.Docs.Terminal_Node with private;

   overriding function Text (Item : Fake_Terminal) return String;

   overriding procedure Set_Position
     (Item   : in out Fake_Terminal;
      Offset : Natural;
      Line   : Positive;
      Column : Positive);

   function Offset (Item : Fake_Terminal) return Natural;
   function Line (Item : Fake_Terminal) return Natural;
   function Column (Item : Fake_Terminal) return Natural;

   type Fake_Terminal_Access is access all Fake_Terminal;

   function Make (S : String) return Fake_Terminal_Access;
   --  A freshly allocated Fake_Terminal holding S.

   function Leaf (T : Fake_Terminal_Access) return Aquarius.Docs.Doc;
   --  Aquarius.Docs.Leaf, with the Fake_Terminal_Access -> Terminal_Node
   --  interface conversion done once here.

private

   type Fake_Terminal is new Aquarius.Docs.Terminal_Node with record
      Content : Ada.Strings.Unbounded.Unbounded_String;
      Offset  : Natural := 0;
      Line    : Natural := 0;
      Column  : Natural := 0;
   end record;

end Fake_Terminals;
