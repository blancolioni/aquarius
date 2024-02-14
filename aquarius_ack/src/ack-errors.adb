with Aquarius.Errors;
with Aquarius.Messages.Console;
with Aquarius.Programs;

package body Ack.Errors is

   Local_Has_Errors : Boolean := False;

   function Error_Message
     (Node    : Node_Id;
      Error   : Error_Kind)
      return String;

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Local_Has_Errors := False;
   end Clear_Errors;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Node    : Node_Id;
      Error   : Error_Kind)
      return String
   is
   begin
      case Error is
         when E_No_Error               =>
            return "no error";
         when E_Undeclared_Name        =>
            if Get_Error_Entity (Node) /= null then
               return To_String (Get_Name (Node))
                 & " not declared in "
                 & Get_Error_Entity (Node).Description;
            else
               return "undeclared: "
                 & To_String (Get_Name (Node));
            end if;
         when E_Redefined_Name =>
            return "redefinition of "
              & Get_Entity (Node).Description;
         when E_Not_Defined_In =>
            return To_String (Get_Name (Node))
              & " is not defined in "
              & Get_Error_Entity (Node).Description;
         when E_Not_A_Create_Feature =>
            return To_String (Get_Name (Node))
              & " is not a creator for "
              & Get_Error_Entity (Node).Description;
         when E_Create_Deferred_Class =>
            return "cannot create instance of deferred class";
         when E_Inherited_Expanded_Class =>
            return "cannot inherit expanded class";
         when E_Requires_Body =>
            return "routine feature requires a body";
         when E_Unnecessary_Redefine =>
            return "deferred feature '"
              & Get_Error_Entity (Node).Declared_Name
              & "' should not appear in redefine";
         when E_Missing_Redefine =>
            return "feature '"
              & Get_Error_Entity (Node).Declared_Name
              & "' must appear in redefine clause for class "
              & Get_Error_Context (Node).Qualified_Name;
         when E_Missing_Redefinition =>
            return "missing declaration for redefined feature "
              & To_String (Get_Name (Node));
         when E_Missing_Exit_Condition =>
            return "loop requires an exit condition";
         when E_No_Child               =>
            return "child class not found";
         when E_No_Component           =>
            return "invalid prefix in selected "
              & "component";
         when E_Id_List_With_Arguments =>
            return "arguments cannot appear here";
         when E_Id_List_With_No_Type   =>
               return "declaration group "
              & "requires a type";
         when E_Id_List_With_Routine   =>
            return "routine can have only one name";
         when E_Type_Error             =>
            declare
               Value_Type : constant Entity_Type :=
                              (if Has_Entity (Node)
                               then Get_Entity (Node).Value_Type
                               else null);
            begin
               return "expected type derived from "
                 & Get_Error_Entity (Node).Description
                 & " but found "
                 & (if Has_Entity (Node)
                    then Get_Entity (Node).Declared_Name
                    else "[no entity]")
                 & (if Value_Type = null then " with no type"
                    else " of type " & Value_Type.Description);
            end;
         when E_Creation_Type_Error =>
            return "non-conforming creation type "
              & Get_Error_Entity (Node).Description;
         when E_Iterator_Type_Error =>
            return "cannot iterate over type "
              & Get_Error_Entity (Node).Description;
         when E_No_Default_Create_Routine =>
            return "explict create call required";
         when E_Insufficient_Arguments =>
            return "not enough arguments";
         when E_Ignored_Return_Value =>
            return "cannot ignore return value of routine";
         when E_Too_Many_Arguments =>
            return "too many arguments";
         when E_Does_Not_Accept_Arguments =>
            return Get_Error_Entity (Node).Declared_Name
              & " declared at "
              & Get_Program (Get_Error_Entity (Node).Declaration_Node)
              .Show_Location
              & " does not accept any arguments";
         when E_Requires_Value =>
            return "feature requires a body";
         when E_Requires_Definition =>
            return Get_Entity (Node).Declared_Name
              & " must implement deferred feature "
              & Get_Error_Entity (Node).Description;
         when E_Illegal_Redefinition =>
            return "illegal redefinition of "
              & Get_Error_Entity (Node).Declared_Name;
         when E_Not_An_Iterator =>
            return "type " & Get_Error_Entity (Node).Declared_Name
              & " is not traversable";
         when E_Value_Might_Be_Void =>
            return "value of attached entity "
              & Get_Error_Entity (Node).Declared_Name
              & " might void but there is no default create routine";
      end case;
   end Error_Message;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Local_Has_Errors;
   end Has_Errors;

   -------------------
   -- Record_Errors --
   -------------------

   procedure Record_Errors (Node : Node_Id) is

      procedure Set_Error
        (Node    : Ack.Node_Id;
         Error   : Ack.Error_Kind;
         Warning : Boolean);

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error
        (Node    : Ack.Node_Id;
         Error   : Ack.Error_Kind;
         Warning : Boolean)
      is
         Program : constant Aquarius.Programs.Program_Tree :=
                     Get_Program (Node);
         Warning_Prefix : constant String :=
                            (if Warning
                             then "warning: " else "");
         Message        : constant String := Error_Message (Node, Error);
      begin
         Local_Has_Errors := not Warning;
         Aquarius.Errors.Error (Program, Warning_Prefix & Message);
      end Set_Error;

   begin
      Scan_Errors (Node, Set_Error'Access);
   end Record_Errors;

   -------------------
   -- Report_Errors --
   -------------------

   procedure Report_Errors (Node : Node_Id) is
      use Aquarius.Messages;
      List : Message_List;
   begin
      Get_Program (Node).Get_Messages (List);
      if Message_Count (List) > 0 then
         if Highest_Level (List) >= Warning then
            Aquarius.Messages.Console.Show_Messages (List);
         end if;
      end if;
   end Report_Errors;

end Ack.Errors;
