package body Aquarius.Syntax.Checks is

   ------------
   -- Begins --
   ------------

   function Begins
     (Tok     : Aquarius.Tokens.Token;
      Tree    : Syntax_Tree)
      return Boolean
   is
      use type Aquarius.Tokens.Token;
      Result : Boolean := False;
   begin

      if Tree.Is_Begins_Cached (Tok) then
         --  Ada.Text_IO.Put_Line ("   Begins: cached value");
         --  Ada.Text_IO.Put_Line ("   cached value = " &
         --  Boolean'Image (Tree.Cached_Begins (Tok)));
         return Tree.Cached_Begins (Tok);
      end if;

      case Tree.Syntax_Class is
         when Terminal =>
            Result := Tok = Tree.Token;

         when Choice =>
            Result := False;
            for I in 1 .. Tree.Child_Count loop
               declare
                  Child : constant Syntax_Tree := Tree.Syntax_Child (I);
               begin
                  if Tree = Child then
                     raise Constraint_Error with
                       "Error: child" & I'Img & " of tree " &
                       Tree.Image & " is itself";
                  end if;
                  if Begins (Tok, Child) then
                     Result := True;
                     exit;
                  end if;
               end;
            end loop;

         when Non_Terminal =>
            Result := False;
            for I in 1 .. Tree.Child_Count loop
               declare
                  Child : constant Syntax_Tree := Tree.Syntax_Child (I);
               begin
                  if Tree = Child then
                     raise Constraint_Error with
                       "Error: child" & I'Img & " of tree " &
                       Tree.Image & " is itself";
                  end if;
                  if Begins (Tok, Child) then
                     Result := True;
                     exit;
                  elsif not Nullable (Child) then
                     exit;
                  end if;
               end;
            end loop;
      end case;

      Tree.Set_Begins (Tok, Result);

--        Ada.Text_IO.Put_Line ("Begins: " & Boolean'Image (Result) &
--                                ": " & Aquarius.Tokens.Image (Tok) &
--                                ": " & Tree.Image);

      return Result;
   end Begins;

   --------------
   -- Nullable --
   --------------

   function Nullable (Tree  : Syntax_Tree) return Boolean is
   begin
      case Tree.Syntax_Class is
         when Terminal =>
            return False;

         when Choice =>

            for I in 1 .. Tree.Child_Count loop
               if Nullable (Tree.Syntax_Child (I)) then
                  return True;
               end if;
            end loop;
            return False;

         when Non_Terminal =>
            if Tree.Optional then
               return True;
            else
               for I in 1 .. Tree.Child_Count loop
                  if not Nullable (Tree.Syntax_Child (I)) then
                     return False;
                  end if;
               end loop;
               return True;
            end if;
      end case;
   end Nullable;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (Test  : access Aquarius.Trees.Root_Tree_Type'Class;
                       Tree  : Syntax_Tree)
                      return Boolean
   is
   begin
      --  if Tree is a Choice, at least one of the children must have a
      --  precondition satisfied by Test

      if Tree.Syntax_Class = Choice then
         for I in 1 .. Tree.Child_Count loop
            if Tree.Syntax_Child (I).Check_Precondition (Test) then
               return True;
            end if;
         end loop;
         return False;
      end if;
      return True;
   end Satisfies;

end Aquarius.Syntax.Checks;
