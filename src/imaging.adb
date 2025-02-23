with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.SQL;      use GNATCOLL.SQL;
with GNATCOLL.SQL.Sqlite;
with Database;          use Database;
with Error_Handling;    use Error_Handling;

package body Imaging is

   procedure Initialize (Database : String) is
   begin
      Db_Descr := GNATCOLL.SQL.Sqlite.Setup (Database);
      Db_Conn := Db_Descr.Build_Connection;
   end Initialize;

   procedure Image (Root_Folder_Title : String) is
      Result  : Forward_Cursor;
      Query   : constant SQL_Query :=
        SQL_Select
          (Fields => Moz_Bookmarks.Id,
           From   => Moz_Bookmarks,
           Where  => Like (Moz_Bookmarks.Title, Root_Folder_Title));
      Root_Folder_Id : Natural;
   begin
      Result.Fetch (Db_Conn, Query);

      if Success (Db_Conn) then
         while Has_Row (Result) loop
            Root_Folder_Id := Integer_Value (Result, 0);
            Next (Result);
         end loop;
         if Processed_Rows (Result) = 1 then
            Put_Line ("./" & Root_Folder_Title);
            Recursive_Image (Root_Folder_Id);
            New_Line;
         elsif Processed_Rows (Result) > 1 then
            Panic ("Given name of root folder is not unique");
         elsif Processed_Rows (Result) = 0 then
            Panic ("Given folder not found");
         end if;
      else
         Panic ("Database query failed");
      end if;
   end Image;

   procedure Image (Root_Folder_Id : Natural) is
      Result  : Forward_Cursor;
      Query   : constant SQL_Query :=
         SQL_Select
            (Fields => Moz_Bookmarks.Title,
            From   => Moz_Bookmarks,
            Where  => Moz_Bookmarks.Id = Root_Folder_Id);
      Root_Folder_Title : Unbounded_String;
   begin
      Result.Fetch (Db_Conn, Query);

      if Success (Db_Conn) then
         while Has_Row (Result) loop
            Root_Folder_Title := To_Unbounded_String (Value (Result, 0));
            Next (Result);
         end loop;
         if Processed_Rows (Result) = 1 then
            Put_Line ("./" & To_String (Root_Folder_Title));
            Recursive_Image (Root_Folder_Id);
            New_Line;
         elsif Processed_Rows (Result) = 0 then
            Panic ("Given folder not found");
         end if;
      else
         Panic ("Database query failed");
      end if;
   end Image;

   procedure Recursive_Image (Root_Id : Natural) is
      Result : Forward_Cursor;
      Query  : constant SQL_Query :=
        SQL_Select
          (Fields =>
             Moz_Bookmarks.Id
             & Moz_Bookmarks.Type_Value
             & Moz_Bookmarks.Parent
             & Moz_Bookmarks.Title
             & Moz_Places.Url,
           From   =>
             Left_Join
               (Moz_Bookmarks, Moz_Places, Moz_Bookmarks.Fk = Moz_Places.Id),
           Where  => Moz_Bookmarks.Parent = Root_Id);
   begin
      Result.Fetch (Db_Conn, Query);

      if Success (Db_Conn) then
         while Has_Row (Result) loop
            if Integer_Value (Result, 1) = 1 then
               Put_Line (Value (Result, 4));
            else
               Put_Line ("./" & Value (Result, 3));
               Recursive_Image (Integer_Value (Result, 0));
               New_Line;
            end if;
            Next (Result);
         end loop;
      else
         Panic ("Database query failed");
      end if;
   end Recursive_Image;

   procedure Clean_Up is
   begin
      Free (Db_Conn);
      Free (Db_Descr);
   end Clean_Up;

end Imaging;
