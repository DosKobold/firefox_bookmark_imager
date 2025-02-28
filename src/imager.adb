with Ada.Text_IO;    use Ada.Text_IO;
with Database;       use Database;
with Error_Handling; use Error_Handling;
with GNATCOLL.SQL;   use GNATCOLL.SQL;
with GNATCOLL.SQL.Sqlite;

package body Imager is

   procedure Initialize (Given_Descr : Image_Description) is
   begin
      Img_Descr := Given_Descr;
      Db_Descr := GNATCOLL.SQL.Sqlite.Setup (To_String (Img_Descr.Database));
      Db_Conn := Db_Descr.Build_Connection;
   end Initialize;

   procedure Image (Root_Folder_Title : String) is
      Result         : Forward_Cursor;
      Query          : constant SQL_Query :=
        SQL_Select
          (Fields => Moz_Bookmarks.Id,
           From   => Moz_Bookmarks,
           Where  => Like (Moz_Bookmarks.Title, Root_Folder_Title));
      Root_Folder_Id : Natural;
   begin
      Result.Fetch (Db_Conn, Query);

      Check_Query_Success;

      while Result.Has_Row loop
         Root_Folder_Id := Result.Integer_Value (0);
         Result.Next;
      end loop;

      Image_Root_Folder
        (Result.Processed_Rows, Root_Folder_Id, Root_Folder_Title);
   end Image;

   procedure Image (Root_Folder_Id : Natural) is
      Result            : Forward_Cursor;
      Query             : constant SQL_Query :=
        SQL_Select
          (Fields => Moz_Bookmarks.Title,
           From   => Moz_Bookmarks,
           Where  => Moz_Bookmarks.Id = Root_Folder_Id);
      Root_Folder_Title : Unbounded_String;
   begin
      Result.Fetch (Db_Conn, Query);

      Check_Query_Success;

      while Result.Has_Row loop
         Root_Folder_Title := To_Unbounded_String (Result.Value (0));
         Result.Next;
      end loop;

      Image_Root_Folder
        (Result.Processed_Rows, Root_Folder_Id, To_String (Root_Folder_Title));
   end Image;

   procedure Clean_Up is
   begin
      Free (Db_Conn);
      Free (Db_Descr);
   end Clean_Up;

   ----------------------------------------------------------------------------

   procedure Image_Root_Folder
     (Found_Folders : Natural; Id : Natural; Title : String) is
   begin
      case Found_Folders is
         when 0 =>
            Panic (Error_Folder_Not_Found);

         when 1 =>
            Print_Folder (Id, Title, 1);

         when others =>
            Panic (Error_Folder_Not_Unique);
      end case;
   end Image_Root_Folder;

   procedure Recursive_Image (Root_Id : Natural; Current_Depth : Positive) is
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

      Objects : String_Sets.Set;
      Folders : String_Sets.Set;
   begin
      if Current_Depth > Img_Descr.Tree_Depth then
         Panic
           (Error_Max_Tree_Depth,
            "Current value: " & Img_Descr.Tree_Depth'Image);
      end if;

      Result.Fetch (Db_Conn, Query);

      Check_Query_Success;

      while Result.Has_Row loop
         case Result.Integer_Value (1) is
            when Type_Object =>
               if not Img_Descr.Doubles then
                  Check_For_Doubles (Objects, Result.Value (4));
               end if;

               Print_Object (Result.Value (4));

            when Type_Folder =>
               if not Img_Descr.Doubles then
                  Check_For_Doubles (Folders, Result.Value (3));
               end if;

               Print_Folder
                 (Result.Integer_Value (0), Result.Value (3), Current_Depth);

            when others =>
               Panic
                 (Error_Unknown_Type,
                  "Type: " & Result.Integer_Value (1)'Image);
         end case;

         Result.Next;
      end loop;
   end Recursive_Image;

   procedure Check_Query_Success is
   begin
      if not Db_Conn.Success then
         Panic (Error_Sql_Query, Db_Conn.Error);
      end if;
   end Check_Query_Success;

   procedure Check_For_Doubles
     (Elements : in out String_Sets.Set; Content : String) is
   begin
      begin
         Elements.Insert (To_Unbounded_String (Content));
      exception
         when Constraint_Error =>
            Panic (Error_Doubled_Elements, "Value: " & Content);
      end;
   end Check_For_Doubles;

   procedure Print_Folder (Id : Natural; Name : String; Depth : Positive) is
   begin
      Put_Line (To_String (Img_Descr.Folder_Pre) & Name);
      Recursive_Image (Id, Depth);
      Put_Line (To_String (Img_Descr.Folder_Post));
   end Print_Folder;

   procedure Print_Object (Content : String) is
   begin
      Put_Line
        (To_String (Img_Descr.Object_Pre)
         & Content
         & To_String (Img_Descr.Object_Post));
   end Print_Object;

end Imager;
