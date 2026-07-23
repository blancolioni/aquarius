with Aquarius.Programs;

--  Toolkit-neutral styling for rendered program trees. A Styler maps a
--  terminal's render class (the Class string passed to Renderer.Set_Text) to a
--  Text_Style (colour + font attributes). A concrete renderer (e.g. the GtkAda
--  source view) translates Text_Style into its own toolkit primitives.
--
--  Colours are expressed as fractions in 0.0 .. 1.0 so no toolkit type leaks
--  into this layer.

package Aquarius.Rendering.Styles is

   type Colour_Component is new Float range 0.0 .. 1.0;

   type Colour is record
      Red, Green, Blue : Colour_Component;
   end record;

   type Text_Style is record
      Foreground     : Colour   := (0.0, 0.0, 0.0);
      Has_Background : Boolean  := False;
      Background     : Colour   := (1.0, 1.0, 1.0);
      Bold           : Boolean  := False;
      Italic         : Boolean  := False;
      Underline      : Boolean  := False;
   end record;

   type Styler_Interface is interface;

   function Style_For
     (Styler   : Styler_Interface;
      Class    : String;
      Terminal : Aquarius.Programs.Program_Tree)
      return Text_Style is abstract;
   --  Visual style for a terminal of the given render Class. Terminal is
   --  supplied for context-sensitive styling (ancestry, declaration/reference,
   --  cross-references); the default styler keys on Class alone.

   type Styler_Reference is access all Styler_Interface'Class;

   function Default_Styler return Styler_Reference;
   --  A single built-in theme suitable for a light bubble background.

end Aquarius.Rendering.Styles;
