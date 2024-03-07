with Ada.Exceptions;
with Ada.Text_IO;

with Aquarius.Names;

package body Aquarius.Actions is

   Trace_Actions : constant Boolean := False;

   type Action_Group_Record is
      record
         --  Index         : Positive;
         Group_Name    : Aquarius.Names.Aquarius_Name;
         Group_Trigger : Action_Execution_Trigger;
      end record;

   type Built_In_Action_Execution is
     new Action_Execution_Interface with
      record
         Node : Node_Action;
         Parent : Parent_Action;
      end record;

   overriding procedure Execute
     (Executor : Built_In_Action_Execution;
      Item     : not null access Actionable'Class);

   overriding procedure Execute
     (Executor : Built_In_Action_Execution;
      Parent   : not null access Actionable'Class;
      Child    : not null access Actionable'Class);

   -----------------------
   -- Action_Group_Name --
   -----------------------

   function Action_Group_Name (Group : Action_Group) return String is
   begin
      return Aquarius.Names.To_String (Group.Group_Name);
   end Action_Group_Name;

   --------------------------
   -- Action_Group_Trigger --
   --------------------------

   function Action_Group_Trigger (Group : Action_Group)
                                 return Action_Execution_Trigger
   is
   begin
      return Group.Group_Trigger;
   end Action_Group_Trigger;

   ----------------------
   -- Add_Action_Group --
   ----------------------

   procedure Add_Action_Group
     (List       : in out Action_Group_List;
      Group      : Action_Group)
   is
   begin
      List.Groups.Append (Group);
   end Add_Action_Group;

   -----------------
   -- After_Child --
   -----------------

   function After_Child (Child : not null access Action_Source'Class)
                        return Action_Position
   is
   begin
      return (After, Child_Relative, Child);
   end After_Child;

   ----------------
   -- After_Node --
   ----------------

   function After_Node return Action_Position is
   begin
      return (After, Node_Relative, null);
   end After_Node;

   ------------
   -- Append --
   ------------

   procedure Append (List   : in out Action_Instance_List;
                     Action : Action_Instance)
   is
   begin
      List.List.Append (Action);
   end Append;

   ------------------
   -- Before_Child --
   ------------------

   function Before_Child (Child : not null access Action_Source'Class)
                         return Action_Position
   is
   begin
      return (Before, Child_Relative, Child);
   end Before_Child;

   ----------------
   -- Before_Node --
   ----------------

   function Before_Node return Action_Position is
   begin
      return (Before, Node_Relative, null);
   end Before_Node;

   -----------------------------
   -- Create_Action_Execution --
   -----------------------------

   function Create_Action_Execution
     (Action : Node_Action)
      return Action_Execution_Interface'Class
   is
   begin
      return Built_In_Action_Execution'(Node => Action, Parent => null);
   end Create_Action_Execution;

   -----------------------------
   -- Create_Action_Execution --
   -----------------------------

   function Create_Action_Execution
     (Action : Parent_Action)
      return Action_Execution_Interface'Class
   is
   begin
      return Built_In_Action_Execution'(Node => null, Parent => Action);
   end Create_Action_Execution;

   -------------------------
   -- Create_Action_Group --
   -------------------------

   procedure Create_Action_Group
     (List       : in out Action_Group_List;
      Group_Name : String;
      Trigger    : Action_Execution_Trigger;
      Group      :    out Action_Group)
   is
   begin
      Group := new Action_Group_Record'
        ( --  Index         => List.Groups.Last_Index + 1,
         Group_Name    => Aquarius.Names.To_Aquarius_Name (Group_Name),
         Group_Trigger => Trigger);
      List.Groups.Append (Group);
   end Create_Action_Group;

   -----------------------------
   -- Empty_Action_Group_List --
   -----------------------------

   function Empty_Action_Group_List return Action_Group_List is
      Result : Action_Group_List;
   begin
      return Result;
   end Empty_Action_Group_List;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Built_In_Action_Execution;
      Item     : not null access Actionable'Class)
   is
   begin
      Executor.Node (Item);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Built_In_Action_Execution;
      Parent   : not null access Actionable'Class;
      Child    : not null access Actionable'Class)
   is
   begin
      Executor.Parent (Parent, Child);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Source   : in out Action_Source'Class;
                      Target   : in out Actionable'Class;
                      Group    : Action_Group;
                      Position : Rule_Position)
   is
      Instances : constant Action_Instance_List :=
        Source.Get_Action_List;

      --  if Parent_Actions is True, only execute parent
      --  actions.  Otherwise, only execute node actions.
      procedure Exec (Parent_Actions : Boolean);

      ----------
      -- Exec --
      ----------

      procedure Exec (Parent_Actions : Boolean) is
      begin

         for Instance of Instances.List loop
            if Instance.Position = Position and then
              Instance.Group = Group
            then
               if Parent_Actions and then Instance.Parent /= null then
                  declare
                     Parent : constant access Actionable'Class :=
                                Target.Parent_Actionable (Instance.Parent);
                  begin
                     if Parent /= null then
                        if Trace_Actions then
                           Ada.Text_IO.Put_Line
                             ("action: " & Parent.Name & "/"
                              & Target.Name);
                        end if;
                        Instance.Action.Execute (Parent, Target'Access);
                     end if;
                  end;
               elsif not Parent_Actions and then Instance.Parent = null then
                  if Trace_Actions then
                     Ada.Text_IO.Put_Line
                       ("action: " & Target.Name);
                  end if;
                  Instance.Action.Execute (Target'Access);
               end if;
            end if;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "caught exception while running actions");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "  target: " & Target.Image);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "   exception message: " &
                 Ada.Exceptions.Exception_Message (E));
            raise;
      end Exec;

   begin

      --  if position is before, then parent actions
      --  have priority, otherwise node actions do

      if Position = Before then
         Exec (True);
         Exec (False);
      else
         Exec (False);
         Exec (True);
      end if;

   end Execute;

   ---------------
   -- Get_Group --
   ---------------

--     function Get_Group (List  : Action_Group_List;
--                         Index : Positive)
--                        return Action_Group
--     is
--     begin
--        return List.Groups.Element (Index);
--     end Get_Group;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group (List        : Action_Group_List;
                       Group_Name  : String)
                       return Action_Group
   is
      use type Aquarius.Names.Aquarius_Name;
   begin
      for Group of List.Groups loop
         if Group.Group_Name = Group_Name then
            return Group;
         end if;
      end loop;

      raise Constraint_Error with
        "expected to find a group called '" & Group_Name & "'";
   end Get_Group;

   ----------------
   -- Have_Group --
   ----------------

   function Have_Group
     (List        : Action_Group_List;
      Group_Name  : String)
      return Boolean
   is
      use type Aquarius.Names.Aquarius_Name;
   begin
      for Group of List.Groups loop
         if Group.Group_Name = Group_Name then
            return True;
         end if;
      end loop;

      return False;
   end Have_Group;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (List    : Action_Group_List;
      Trigger : Action_Execution_Trigger;
      Process : not null access
        procedure (Group : Action_Group))
   is
   begin
      for Group of List.Groups loop
         if Group.Group_Trigger = Trigger then
            Process (Group);
         end if;
      end loop;
   end Iterate;

   -----------------------
   -- New_Instance_List --
   -----------------------

--     function New_Instance_List return Action_Instance_List is
--     begin
--        return new Action_Instance_Vector.Vector;
--     end New_Instance_List;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Group    : Action_Group;
                         Position : Rule_Position;
                         Action   : Action_Execution_Interface'Class)
   is
      Instance  : constant Action_Instance :=
                    (Group    => Group,
                     Parent   => null,
                     Position => Position,
                     Action   =>
                        new Action_Execution_Interface'Class'(Action));
   begin
      Source.Append_Action (Instance);
   end Set_Action;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Child    : not null access Action_Source'Class;
                         Group    : Action_Group;
                         Position : Rule_Position;
                         Action   : Action_Execution_Interface'Class)
   is
      Instance  : constant Action_Instance :=
                    (Group       => Group,
                     Position    => Position,
                     Parent      => Source,
                     Action      =>
                        new Action_Execution_Interface'Class'(Action));
   begin
      Child.Append_Action (Instance);
   end Set_Action;

   ----------
   -- Show --
   ----------

   function Show (Position : Action_Position) return String is
   begin
      case Position.Anchor is
         when Node_Relative =>
            case Position.Pos_Type is
               when Before =>
                  return "before node";
               when After =>
                  return "after node";
            end case;
         when Child_Relative =>
            case Position.Pos_Type is
               when Before =>
                  return "before child '" & Position.Child.Name & "'";
               when After =>
                  return "after child '" & Position.Child.Name & "'";
            end case;
      end case;
   end Show;

end Aquarius.Actions;
