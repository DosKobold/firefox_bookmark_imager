with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;
with Error_Handling;   use Error_Handling;
with Imaging;
with Parse_Args;       use Parse_Args;

procedure Main is
   Folder_Id  : Natural;
   Arg_Parser : Argument_Parser;

   procedure Initialize_Arg_Parser is
   begin
      Arg_Parser.Add_Option
        (Make_Boolean_Option (False),
         "help",
         'h',
         Usage => "Display this help text");
      Arg_Parser.Add_Option
        (Make_Boolean_Option (False),
         "id-as-root",
         'i',
         Usage => "Use the id for the root folder");
      Arg_Parser.Add_Option
        (Make_Boolean_Option (True),
         "doubles",
         'd',
         Usage => "Disable doubles in the folders");
      Arg_Parser.Add_Option
        (Make_Positive_Option (50),
         "tree-depth",
         't',
         Usage =>
           "Change the tree depth for recursive algorithm. Default: 50");
      Arg_Parser.Add_Option
        (Make_String_Option ("./"),
         "folder-pre",
         Usage => "Change the pre marker of folders");
      Arg_Parser.Add_Option
        (Make_String_Option (""),
         "folder-post",
         Usage => "Change the post marker of folders");
      Arg_Parser.Add_Option
        (Make_String_Option (""),
         "object-pre",
         Usage => "Change the pre marker of objects");
      Arg_Parser.Add_Option
        (Make_String_Option (""),
         "object-post",
         Usage => "Change the post marker of objects");
      Arg_Parser.Append_Positional (Make_String_Option, "DATABASE");
      Arg_Parser.Append_Positional (Make_String_Option, "ROOTFOLDER");
      Arg_Parser.Parse_Command_Line;
   end Initialize_Arg_Parser;
begin
   Initialize_Arg_Parser;

   if not Arg_Parser.Parse_Success then
      Panic (Error_Parsing_Arguments, Arg_Parser.Parse_Message);
   end if;

   if Arg_Parser.Boolean_Value ("help") then
      Arg_Parser.Usage;

      goto Successfull_End_Of_Program;
   end if;

   if Arg_Parser.String_Value ("DATABASE")'Length = 0
     or else Arg_Parser.String_Value ("ROOTFOLDER")'Length = 0
   then
      Panic (Error_Missing_Arguments);
   end if;

   if not Exists (Arg_Parser.String_Value ("DATABASE")) then
      Panic (Error_File_Not_Found);
   end if;

   Imaging.Initialize
     (Arg_Parser.String_Value ("DATABASE"),
      Arg_Parser.Boolean_Value ("doubles"),
      Positive (Arg_Parser.Integer_Value ("tree-depth")),
      Arg_Parser.String_Value ("folder-pre"),
      Arg_Parser.String_Value ("folder-post"),
      Arg_Parser.String_Value ("object-pre"),
      Arg_Parser.String_Value ("object-post"));

   if Arg_Parser.Boolean_Value ("id-as-root") then
      begin
         Folder_Id := Natural'Value (Arg_Parser.String_Value ("ROOTFOLDER"));
      exception
         when Constraint_Error =>
            Panic (Error_Parsing_Root_Id);
      end;

      Imaging.Image (Folder_Id);
   else
      Imaging.Image (Arg_Parser.String_Value ("ROOTFOLDER"));
   end if;

   Imaging.Clean_Up;

   <<Successfull_End_Of_Program>>
   Set_Exit_Status (0);
end Main;
