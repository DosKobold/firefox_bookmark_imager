with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Error_Handling is

   type Error is private;

   procedure Panic (Class : Error; Message : String := "");

   Error_General           : constant Error;
   Error_Parsing_Arguments : constant Error;
   Error_Missing_Arguments : constant Error;
   Error_Parsing_Root_Id   : constant Error;
   Error_File_Not_Found    : constant Error;
   Error_Sql_Query         : constant Error;
   Error_Folder_Not_Found  : constant Error;
   Error_Folder_Not_Unique : constant Error;
   Error_Max_Tree_Depth    : constant Error;
   Error_Doubled_Elements  : constant Error;
   Error_Unknown_Type      : constant Error;

private

   type Error is record
      Id          : Integer;
      Description : Unbounded_String;
      Hints       : Unbounded_String;
   end record;

   function New_Error
     (Id : Integer; Description : String; Hints : String := "") return Error
   is ((Id, To_Unbounded_String (Description), To_Unbounded_String (Hints)));

   Error_General           : constant Error :=
     New_Error (1, "Unknown error occured");
   Error_Parsing_Arguments : constant Error :=
     New_Error
       (2, "Parsing of command line arguments failed", "Use ""-h"" for help");
   Error_Missing_Arguments : constant Error :=
     New_Error (2, "Missing required argument(s)", "Use ""-h"" for help");
   Error_Parsing_Root_Id   : constant Error :=
     New_Error
       (2,
        "Cannot parse the id of the root folder",
        "Is it a positve number?");
   Error_File_Not_Found    : constant Error :=
     New_Error (3, "File not found", "Wrong path?");
   Error_Sql_Query         : constant Error :=
     New_Error (4, "Database query failed", "Is the given file a database?");
   Error_Folder_Not_Found  : constant Error :=
     New_Error (5, "Given folder not found", "Wrong folder name?");
   Error_Folder_Not_Unique : constant Error :=
     New_Error (5, "Given folder is not unique", "Wrong folder name?");
   Error_Max_Tree_Depth    : constant Error :=
     New_Error
       (6,
        "Maximum tree depth in recursive algorithm reached",
        "Changeable with ""-t""");
   Error_Doubled_Elements  : constant Error :=
     New_Error
       (7,
        "Some folder contains two elements with the same value",
        "Changeable with ""-d""");
   Error_Unknown_Type      : constant Error :=
     New_Error (7, "Unknown type of element found", "");

end Error_Handling;
