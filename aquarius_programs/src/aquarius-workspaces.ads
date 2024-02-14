private with Aquarius.Names;
private with WL.String_Maps;

with Aquarius.Programs;

package Aquarius.Workspaces is

   type Instance is
     new Aquarius.Programs.Root_Program_Tree_Store
   with private;

   type Reference is access all Instance'Class;

   function New_Empty_Workspace
     (Path : String)
      return Reference;

private

   package Program_Maps is
     new WL.String_Maps (Aquarius.Programs.Program_Tree,
                         Aquarius.Programs."=");

   type Instance is
     new Aquarius.Programs.Root_Program_Tree_Store with
      record
         Name : Aquarius.Names.Aquarius_Name;
         Path : Aquarius.Names.Aquarius_Name;
         Map  : Program_Maps.Map;
      end record;

   overriding function Environment_Name
     (This : Instance)
      return String
   is (Aquarius.Names.To_String (This.Name));

   overriding function Get_Program
     (This      : Instance;
      File_Name : String)
      return Aquarius.Programs.Program_Tree;

end Aquarius.Workspaces;
