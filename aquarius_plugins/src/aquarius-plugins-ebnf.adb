with Aquarius.Plugins.EBNF.Analyse;

package body Aquarius.Plugins.EBNF is

   Global_EBNF_Plugin : Aquarius_Plugin;

   -------------------
   -- Global_Plugin --
   -------------------

   function Global_Plugin return access EBNF_Plugin'Class is
   begin
      return EBNF_Plugin (Global_EBNF_Plugin.all)'Access;
   end Global_Plugin;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load (Plugin  : not null access EBNF_Plugin;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
      Analysis : Aquarius.Actions.Action_Group;
   begin
      Aquarius.Properties.Create_Property (Pool      => Grammar.all,
                                           Prop      => Plugin.Separator,
                                           Name      => "ebnf-separator",
                                           Inherited => False,
                                           Has_Value => True);

      Load (Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Plugin.Register_Rule
        ("definition",
         Aquarius.Formats.New_Line (After));
      Plugin.Register_Rule
        ("::=",
         Aquarius.Formats.New_Line (After));
      Plugin.Register_Rule
        ("definition-body",
         Aquarius.Formats.Indent_Child (8));
      Plugin.Register_Rule
        ("|",
         Aquarius.Formats.New_Line (Before));
      Plugin.Register_Rule
        ("<",
         Aquarius.Formats.Space_Always (Before),
         Aquarius.Formats.Space_Never (After));
      Plugin.Register_Rule
        (">",
         Aquarius.Formats.Space_Always (After),
         Aquarius.Formats.Space_Never (Before));

      Grammar.Add_Action_Group ("analyse",
                                Aquarius.Actions.Manual_Trigger,
                                Analysis);

      Plugin.Register_Action
        ("rule-definition", Analysis, Before,
         Analyse.Before_Rule_Definition'Access);

      Plugin.Register_Action
        ("rule-definition", Analysis, After,
         Analyse.After_Rule_Definition'Access);

      Plugin.Register_Action
        ("value-definition", Analysis, Before,
         Analyse.Before_Value_Definition'Access);

      Plugin.Register_Action
        ("value-definition", Analysis, After,
         Analyse.After_Value_Definition'Access);

      Plugin.Register_Action
        ("format-definition", Analysis, Before,
         Analyse.Before_Format_Definition'Access);

      Plugin.Register_Action
        ("format-definition", Analysis, After,
         Analyse.After_Format_Definition'Access);

      Plugin.Register_Action
        ("xref-definition", Analysis, After,
         Analyse.After_Cross_Reference_Definition'Access);

      Plugin.Register_Action
        ("syntax-body", Analysis, After,
         Analyse.After_Syntax_Body'Access);

      Plugin.Register_Action
        ("sequence-of-rules", Analysis, After,
         Analyse.After_Sequence_Of_Rules'Access);

      Plugin.Register_Action
        ("optional_when", Analysis, After,
         Analyse.After_When'Access);

      Plugin.Register_Action
        ("rule", Analysis, After,
         Analyse.After_Rule'Access);

      Plugin.Register_Action
        ("repeat-optional-rule", Analysis, After,
         Analyse.After_Repeat_Optional_Rule'Access);

      Plugin.Register_Action
        ("repeat-required-rule", Analysis, After,
         Analyse.After_Repeat_Required_Rule'Access);

      Plugin.Register_Action
        ("optional-rule", Analysis, After,
         Analyse.After_Optional_Rule'Access);

      Plugin.Register_Action
        ("nested-rule", Analysis, After,
         Analyse.After_Nested_Rule'Access);

      Plugin.Register_Action
        ("repeater", Analysis, After,
         Analyse.After_Repeater'Access);

      Plugin.Register_Action
        ("separator", Analysis, After,
         Analyse.After_Separator'Access);

      Plugin.Register_Action
        ("sequence-rule", Analysis, After,
         Analyse.After_Sequence_Rule'Access);

      Plugin.Register_Action
        ("terminal-rule", Analysis, After,
         Analyse.After_Terminal_Rule'Access);

      Global_EBNF_Plugin := Aquarius_Plugin (Plugin);

   end Load;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : EBNF_Plugin) return String is
      pragma Unreferenced (Plugin);
   begin
      return "EBNF";
   end Name;

   -------------
   -- Version --
   -------------

   overriding
   function Version (Plugin : EBNF_Plugin) return String is
      pragma Unreferenced (Plugin);
   begin
      return "1.0";
   end Version;

end Aquarius.Plugins.EBNF;
