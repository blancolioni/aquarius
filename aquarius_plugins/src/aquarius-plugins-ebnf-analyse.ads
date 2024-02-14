with Aquarius.Actions;

package Aquarius.Plugins.EBNF.Analyse is

   procedure Before_Rule_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Rule_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure Before_Value_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Value_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure Before_Format_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Format_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Cross_Reference_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Syntax_Body
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Sequence_Of_Rules
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_When
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Repeat_Optional_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Repeat_Required_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Optional_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Nested_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Repeater
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Separator
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Sequence_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);
   procedure After_Terminal_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class);

end Aquarius.Plugins.EBNF.Analyse;
