with Ada.Text_IO;

with Ack.Types;

with Ack.Semantic.Analysis.Classes;
with Ack.Semantic.Analysis.Expressions;
with Ack.Semantic.Analysis.Statements;
with Ack.Semantic.Analysis.Types;

package body Ack.Semantic.Analysis is

   -----------------------
   -- Analyse_Assertion --
   -----------------------

   procedure Analyse_Assertion
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assertion  : Node_Id;
      Process    : not null access
        procedure (Tag : Name_Id;
                   Condition : Node_Id))
   is

      procedure Analyse_Clause (Clause : Node_Id);

      --------------------
      -- Analyse_Clause --
      --------------------

      procedure Analyse_Clause (Clause : Node_Id) is
      begin
         Ack.Semantic.Analysis.Expressions.Analyse_Boolean_Expression
           (Class, Container, Attachment, Expression (Clause));
         Process (Get_Name (Clause), Expression (Clause));
      end Analyse_Clause;

   begin
      Scan (Assertion_Clauses (Assertion), Analyse_Clause'Access);
   end Analyse_Assertion;

   -------------------------------
   -- Analyse_Class_Declaration --
   -------------------------------

   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is
   begin
      Ack.Semantic.Analysis.Classes.Analyse_Class_Declaration (Node);
   end Analyse_Class_Declaration;

   -------------------------------
   -- Analyse_Effective_Routine --
   -------------------------------

   procedure Analyse_Effective_Routine
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Routine    : Node_Id)
   is
   begin
      case N_Effective_Routine (Kind (Routine)) is
         when N_Internal =>
            Statements.Analyse_Compound
              (Class, Container, Attachment,
               Compound (Routine));
         when N_External =>
            null;
      end case;
   end Analyse_Effective_Routine;

   ---------------------------------------
   -- Analyse_Entity_Declaration_Groups --
   ---------------------------------------

   procedure Analyse_Entity_Declaration_Groups
     (Class      : Ack.Classes.Class_Entity;
      Feature    : Ack.Features.Feature_Entity;
      Group_List : Node_Id;
      Local      : Boolean)
   is

      procedure Insert_Group (Group_Node : Node_Id);

      ------------------
      -- Insert_Group --
      ------------------

      procedure Insert_Group (Group_Node : Node_Id) is
         Ids         : constant List_Id := Identifiers (Group_Node);
         Type_Node   : constant Node_Id := Group_Type (Group_Node);
         Type_Entity : Ack.Types.Type_Entity := null;

         procedure Insert_Id (Id_Node : Node_Id);

         ---------------
         -- Insert_Id --
         ---------------

         procedure Insert_Id (Id_Node : Node_Id) is
         begin
            if Local then
               Feature.Add_Local
                 (Id_Node, Type_Entity);
            else
               Feature.Add_Argument
                 (Id_Node, Type_Entity);
            end if;
         end Insert_Id;

      begin
         if Type_Node /= No_Node then
            Types.Analyse_Type (Class, Type_Node);
            if Has_Entity (Type_Node) then
               Type_Entity := Ack.Types.Get_Type_Entity (Type_Node);
               Scan (Ids, Insert_Id'Access);
            end if;
         else
            Ada.Text_IO.Put_Line
              ("null type node for "
               & Class.Qualified_Name
               & "." & Feature.Declared_Name
               & " argument");
         end if;

      end Insert_Group;

   begin
      Scan (Node_Table.Element (Group_List).List, Insert_Group'Access);
   end Analyse_Entity_Declaration_Groups;

end Ack.Semantic.Analysis;
