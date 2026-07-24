with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body Aquarius.Models.Trees.Filesystem is

   package Node_Vectors is
     new Ada.Containers.Vectors (Positive, Tree_Node_Reference);

   --  Mutable per-node cache, held behind an access so the enumeration can be
   --  memoised through the interface's by-value (mode in) node parameters.
   type Node_State is record
      Scanned  : Boolean := False;
      Children : Node_Vectors.Vector;
   end record;

   type Node_State_Access is access Node_State;

   type Directory_Node is new Tree_Node_Interface with record
      Full_Path : Unbounded_String;
      Is_Dir    : Boolean;
      State     : Node_State_Access;
   end record;

   type Directory_Node_Access is access all Directory_Node;

   overriding function Label (Node : Directory_Node) return String;
   overriding function Icon_Name (Node : Directory_Node) return String;
   overriding function Has_Children (Node : Directory_Node) return Boolean;
   overriding function Child_Count (Node : Directory_Node) return Natural;
   overriding function Child
     (Node : Directory_Node; Index : Positive) return Tree_Node_Reference;
   overriding function Target (Node : Directory_Node) return String;

   function Make_Node (Path : String) return Tree_Node_Reference;
   procedure Ensure_Scanned (Node : Directory_Node'Class);

   ---------------
   -- Make_Node --
   ---------------

   function Make_Node (Path : String) return Tree_Node_Reference is
      use Ada.Directories;
      Is_Dir : constant Boolean :=
                 Exists (Path) and then Kind (Path) = Directory;
   begin
      return Tree_Node_Reference
        (Directory_Node_Access'
           (new Directory_Node'
              (Full_Path => To_Unbounded_String (Path),
               Is_Dir    => Is_Dir,
               State     => new Node_State)));
   end Make_Node;

   -------------------
   -- Ensure_Scanned --
   -------------------

   procedure Ensure_Scanned (Node : Directory_Node'Class) is
      use Ada.Directories;

      type Entry_Record is record
         Path   : Unbounded_String;
         Name   : Unbounded_String;
         Is_Dir : Boolean;
      end record;

      package Entry_Vectors is
        new Ada.Containers.Vectors (Positive, Entry_Record);

      function Before (Left, Right : Entry_Record) return Boolean is
         use Ada.Characters.Handling;
      begin
         --  Directories first, then case-insensitive alphabetical.
         if Left.Is_Dir /= Right.Is_Dir then
            return Left.Is_Dir;
         end if;
         return To_Lower (To_String (Left.Name))
              < To_Lower (To_String (Right.Name));
      end Before;

      package Entry_Sorting is
        new Entry_Vectors.Generic_Sorting ("<" => Before);

      Entries : Entry_Vectors.Vector;
   begin
      if Node.State.Scanned then
         return;
      end if;
      Node.State.Scanned := True;

      if not Node.Is_Dir then
         return;
      end if;

      declare
         Search : Search_Type;
         Item   : Directory_Entry_Type;
      begin
         Start_Search
           (Search    => Search,
            Directory => To_String (Node.Full_Path),
            Pattern   => "",
            Filter    => [Directory     => True,
                          Ordinary_File => True,
                          Special_File  => False]);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);
            declare
               Name : constant String := Simple_Name (Item);
            begin
               if Name /= "." and then Name /= ".."
                 and then (Name'Length = 0 or else Name (Name'First) /= '.')
               then
                  Entries.Append
                    (Entry_Record'
                       (Path   => To_Unbounded_String (Full_Name (Item)),
                        Name   => To_Unbounded_String (Name),
                        Is_Dir => Kind (Item) = Directory));
               end if;
            end;
         end loop;
         End_Search (Search);
      exception
         when others =>
            --  Unreadable directory: leave it childless rather than crash.
            null;
      end;

      Entry_Sorting.Sort (Entries);

      for E of Entries loop
         Node.State.Children.Append (Make_Node (To_String (E.Path)));
      end loop;
   end Ensure_Scanned;

   -----------
   -- Label --
   -----------

   overriding function Label (Node : Directory_Node) return String is
      use Ada.Directories;
      Path : constant String := To_String (Node.Full_Path);
   begin
      declare
         Name : constant String := Simple_Name (Path);
      begin
         if Name = "" then
            return Path;
         else
            return Name;
         end if;
      end;
   exception
      when others =>
         return Path;
   end Label;

   ---------------
   -- Icon_Name --
   ---------------

   overriding function Icon_Name (Node : Directory_Node) return String is
   begin
      if Node.Is_Dir then
         return "folder";
      else
         return "text-x-generic";
      end if;
   end Icon_Name;

   ------------------
   -- Has_Children --
   ------------------

   overriding function Has_Children (Node : Directory_Node) return Boolean is
   begin
      return Node.Is_Dir;
   end Has_Children;

   -----------------
   -- Child_Count --
   -----------------

   overriding function Child_Count (Node : Directory_Node) return Natural is
   begin
      Ensure_Scanned (Node);
      return Natural (Node.State.Children.Length);
   end Child_Count;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Node : Directory_Node; Index : Positive) return Tree_Node_Reference
   is
   begin
      Ensure_Scanned (Node);
      return Node.State.Children (Index);
   end Child;

   ------------
   -- Target --
   ------------

   overriding function Target (Node : Directory_Node) return String is
   begin
      if Node.Is_Dir then
         return "";
      else
         return To_String (Node.Full_Path);
      end if;
   end Target;

   ------------
   -- Create --
   ------------

   function Create (Path : String) return Filesystem_Tree_Model_Access is
   begin
      return new Filesystem_Tree_Model'
        (Aquarius.Observable.Publisher_Base with
           Root_Node => Make_Node (Path));
   end Create;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Model : Filesystem_Tree_Model) return String is
      pragma Unreferenced (Model);
   begin
      return "tree";
   end Kind;

   --------------------
   -- Default_Viewer --
   --------------------

   overriding function Default_Viewer
     (Model : Filesystem_Tree_Model) return String
   is
      pragma Unreferenced (Model);
   begin
      return "tree";
   end Default_Viewer;

   ----------
   -- Root --
   ----------

   overriding function Root
     (Model : Filesystem_Tree_Model) return Tree_Node_Reference
   is
   begin
      return Model.Root_Node;
   end Root;

end Aquarius.Models.Trees.Filesystem;
