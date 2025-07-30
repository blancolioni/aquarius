with Aquarius.Grammars.Manager;
with Aquarius.Plugins.EBNF.Analyse;

package body Aquarius.Plugins.EBNF is

   ----------
   -- Load --
   ----------

   overriding function Load
     (This : in out Instance;
      Name : String)
      return Boolean
   is
      Analysis : Aquarius.Actions.Action_Group;
   begin
      This.Grammar := Aquarius.Grammars.Manager.Get_Grammar ("ebnf");
      This.Name :=
        Aquarius.Names.To_Aquarius_Name ("ebnf");
      This.Description :=
        Aquarius.Names.To_Aquarius_Name ("EBNF description of ENBF syntax");
      This.Version :=
        Aquarius.Names.To_Aquarius_Name ("0.1.0");

      Aquarius.Properties.Create_Property
        (Pool      => This.Grammar.all,
         Prop      => This.Separator,
         Name      => "ebnf-separator",
         Inherited => False,
         Has_Value => True);

      This.Register_Rule
        ("definition",
         Aquarius.Formats.New_Line (After));
      This.Register_Rule
        ("::=",
         Aquarius.Formats.New_Line (After));
      This.Register_Rule
        ("definition-body",
         Aquarius.Formats.Indent_Child (8));
      This.Register_Rule
        ("|",
         Aquarius.Formats.New_Line (Before));
      This.Register_Rule
        ("<",
         Aquarius.Formats.Space_Always (Before),
         Aquarius.Formats.Space_Never (After));
      This.Register_Rule
        (">",
         Aquarius.Formats.Space_Always (After),
         Aquarius.Formats.Space_Never (Before));

      Analysis :=
        This.New_Action_Group
          ("analyse",
           Aquarius.Actions.Manual_Trigger);

      This.Register_Action
        ("rule-definition", Analysis, Before,
         Analyse.Before_Rule_Definition'Access);

      This.Register_Action
        ("rule-definition", Analysis, After,
         Analyse.After_Rule_Definition'Access);

      This.Register_Action
        ("value-definition", Analysis, Before,
         Analyse.Before_Value_Definition'Access);

      This.Register_Action
        ("value-definition", Analysis, After,
         Analyse.After_Value_Definition'Access);

      This.Register_Action
        ("format-definition", Analysis, Before,
         Analyse.Before_Format_Definition'Access);

      This.Register_Action
        ("format-definition", Analysis, After,
         Analyse.After_Format_Definition'Access);

      This.Register_Action
        ("xref-definition", Analysis, After,
         Analyse.After_Cross_Reference_Definition'Access);

      This.Register_Action
        ("syntax-body", Analysis, After,
         Analyse.After_Syntax_Body'Access);

      This.Register_Action
        ("sequence-of-rules", Analysis, After,
         Analyse.After_Sequence_Of_Rules'Access);

      This.Register_Action
        ("optional_when", Analysis, After,
         Analyse.After_When'Access);

      This.Register_Action
        ("rule", Analysis, After,
         Analyse.After_Rule'Access);

      This.Register_Action
        ("repeat-optional-rule", Analysis, After,
         Analyse.After_Repeat_Optional_Rule'Access);

      This.Register_Action
        ("repeat-required-rule", Analysis, After,
         Analyse.After_Repeat_Required_Rule'Access);

      This.Register_Action
        ("optional-rule", Analysis, After,
         Analyse.After_Optional_Rule'Access);

      This.Register_Action
        ("nested-rule", Analysis, After,
         Analyse.After_Nested_Rule'Access);

      This.Register_Action
        ("repeater", Analysis, After,
         Analyse.After_Repeater'Access);

      This.Register_Action
        ("separator", Analysis, After,
         Analyse.After_Separator'Access);

      This.Register_Action
        ("sequence-rule", Analysis, After,
         Analyse.After_Sequence_Rule'Access);

      This.Register_Action
        ("terminal-rule", Analysis, After,
         Analyse.After_Terminal_Rule'Access);

      Global_EBNF_Plugin := This;

      return True;

   end Load;

end Aquarius.Plugins.EBNF;
