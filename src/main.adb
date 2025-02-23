with Imaging;
with Ada.Command_Line; use Ada.Command_Line;
with Error_Handling;    use Error_Handling;

procedure Main is
   procedure Print_Help is
   begin
      Panic ("Usage: "
            & Command_Name & " <DATABASE> <ROOTFOLDER_NAME | ROOTFOLDER_ID>");
   end Print_Help;
   Folder_Id : Natural;
begin
   if Argument_Count /= 2 then
      Print_Help;
   end if;

   Imaging.Initialize (Argument (1));
   begin
      Folder_Id := Natural'Value (Argument (2));
   exception
      when Constraint_Error =>
         Imaging.Image (Argument (2));
         Imaging.Clean_Up;

         Set_Exit_Status (0);
         return;
   end;
   Imaging.Image (Folder_Id);
   Imaging.Clean_Up;

   Set_Exit_Status (0);

end Main;