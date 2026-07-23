with Ada.Strings.Fixed;

package body Aquarius.Rendering.Styles is

   type Default_Styler_Type is new Styler_Interface with null record;

   overriding function Style_For
     (Styler   : Default_Styler_Type;
      Class    : String;
      Terminal : Aquarius.Programs.Program_Tree)
      return Text_Style;

   The_Default_Styler : constant Styler_Reference := new Default_Styler_Type;

   --  Palette (light background)
   Keyword_Colour    : constant Colour := (0.10, 0.10, 0.60);
   String_Colour     : constant Colour := (0.00, 0.50, 0.00);
   Number_Colour     : constant Colour := (0.60, 0.30, 0.00);
   Comment_Colour    : constant Colour := (0.50, 0.50, 0.50);
   Delimiter_Colour  : constant Colour := (0.30, 0.30, 0.30);
   Normal_Colour     : constant Colour := (0.10, 0.10, 0.10);
   Implied_Colour    : constant Colour := (0.60, 0.60, 0.60);
   Error_Colour      : constant Colour := (0.80, 0.00, 0.00);
   Warning_Colour    : constant Colour := (0.80, 0.50, 0.00);

   function Has_Prefix (S, Prefix : String) return Boolean
   is (S'Length >= Prefix'Length
       and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   function Contains (S, Needle : String) return Boolean
   is (Ada.Strings.Fixed.Index (S, Needle) > 0);

   ---------------
   -- Style_For --
   ---------------

   overriding function Style_For
     (Styler   : Default_Styler_Type;
      Class    : String;
      Terminal : Aquarius.Programs.Program_Tree)
      return Text_Style
   is
      pragma Unreferenced (Styler);
      pragma Unreferenced (Terminal);
   begin
      --  Reserved words: "reserved_<class>".
      if Has_Prefix (Class, "reserved") then
         return (Foreground => Keyword_Colour, Bold => True, others => <>);

      elsif Class = "error" then
         return (Foreground => Error_Colour, Underline => True, others => <>);

      elsif Class = "warning" then
         return (Foreground => Warning_Colour, Underline => True,
                 others => <>);

      elsif Class = "implied_token" then
         return (Foreground => Implied_Colour, Italic => True, others => <>);

      elsif Contains (Class, "comment") then
         return (Foreground => Comment_Colour, Italic => True, others => <>);

      elsif Contains (Class, "string") or else Contains (Class, "character")
      then
         return (Foreground => String_Colour, others => <>);

      elsif Contains (Class, "integer") or else Contains (Class, "number")
        or else Contains (Class, "float") or else Contains (Class, "digit")
      then
         return (Foreground => Number_Colour, others => <>);

      elsif Contains (Class, "delimiter") then
         return (Foreground => Delimiter_Colour, others => <>);

      elsif Class = "normal" or else Contains (Class, "identifier") then
         return (Foreground => Normal_Colour, others => <>);

      else
         --  Grammar-defined render classes (e.g. "function_name") and anything
         --  unrecognised: default foreground.
         return (Foreground => Normal_Colour, others => <>);
      end if;
   end Style_For;

   --------------------
   -- Default_Styler --
   --------------------

   function Default_Styler return Styler_Reference is
   begin
      return The_Default_Styler;
   end Default_Styler;

end Aquarius.Rendering.Styles;
