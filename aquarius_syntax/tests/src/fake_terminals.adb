package body Fake_Terminals is

   ------------
   -- Column --
   ------------

   function Column (Item : Fake_Terminal) return Natural is
   begin
      return Item.Column;
   end Column;

   ----------
   -- Leaf --
   ----------

   function Leaf (T : Fake_Terminal_Access) return Aquarius.Docs.Doc is
   begin
      return Aquarius.Docs.Leaf (Aquarius.Docs.Terminal_Node_Access (T));
   end Leaf;

   ----------
   -- Line --
   ----------

   function Line (Item : Fake_Terminal) return Natural is
   begin
      return Item.Line;
   end Line;

   ----------
   -- Make --
   ----------

   function Make (S : String) return Fake_Terminal_Access is
   begin
      return new Fake_Terminal'
        (Content => Ada.Strings.Unbounded.To_Unbounded_String (S),
         Line    => 0,
         Column  => 0);
   end Make;

   ------------------
   -- Set_Position --
   ------------------

   overriding procedure Set_Position
     (Item   : in out Fake_Terminal;
      Line   : Positive;
      Column : Positive)
   is
   begin
      Item.Line := Line;
      Item.Column := Column;
   end Set_Position;

   ----------
   -- Text --
   ----------

   overriding function Text (Item : Fake_Terminal) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Content);
   end Text;

end Fake_Terminals;
