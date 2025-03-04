with Imager.Description; use Imager.Description;
with Imager.Firefox;
with Imager.Firefox.Sqlite;

package body Imager is

   procedure Initialize
     (Database     : Unbounded_String;
      Check_Syntax : Boolean;
      Doubles      : Boolean;
      Tree_Depth   : Positive;
      Folder_Pre   : Unbounded_String;
      Folder_Post  : Unbounded_String;
      Object_Pre   : Unbounded_String;
      Object_Post  : Unbounded_String) is
   begin
      Img_Descr :=
        (Database,
         Check_Syntax,
         Doubles,
         Tree_Depth,
         Folder_Pre,
         Folder_Post,
         Object_Pre,
         Object_Post);

      Imager.Firefox.Sqlite.Initialize;

      Is_Initialized := True;
   end Initialize;

   procedure Image (Root_Folder_Title : String) is
   begin
      Imager.Firefox.Sqlite.Image (Root_Folder_Title);
   end Image;

   procedure Image (Root_Folder_Id : Natural) is
   begin
      Imager.Firefox.Sqlite.Image (Root_Folder_Id);
   end Image;

   procedure Clean_Up is
   begin
      Imager.Firefox.Sqlite.Clean_Up;
      Is_Initialized := False;
   end Clean_Up;

end Imager;
