with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;    use Ada.Text_IO;
with Database;       use Database;
with Error_Handling; use Error_Handling;
with GNATCOLL.SQL;   use GNATCOLL.SQL;
with GNATCOLL.SQL.Sqlite;

package body Imaging is

   procedure Initialize
     (Database    : String;
      Doubles     : Boolean;
      Tree_Depth  : Positive;
      Folder_Pre  : String;
      Folder_Post : String;
      Object_Pre  : String;
      Object_Post : String) is
   begin
      Db_Descr := GNATCOLL.SQL.Sqlite.Setup (Database);
      Db_Conn := Db_Descr.Build_Connection;
      Allow_Doubles := Doubles;
      Recursion_Depth := Tree_Depth;
      Marker_Folder_Pre := To_Unbounded_String (Folder_Pre);
      Marker_Folder_Post := To_Unbounded_String (Folder_Post);
      Marker_Object_Pre := To_Unbounded_String (Object_Pre);
      Marker_Object_Post := To_Unbounded_String (Object_Post);
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

      if not Db_Conn.Success then
         Panic (Error_Sql_Query, Db_Conn.Error);
      end if;

      while Result.Has_Row loop
         Root_Folder_Id := Result.Integer_Value (0);
         Result.Next;
      end loop;

      case Result.Processed_Rows is
         when 0 =>
            Panic (Error_Folder_Not_Found);

         when 1 =>
            Put_Line (To_String (Marker_Folder_Pre) & Root_Folder_Title);
            Recursive_Image (Root_Folder_Id, 1);
            Put_Line (To_String (Marker_Folder_Post));

         when others =>
            Panic (Error_Folder_Not_Unique);
      end case;
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

      if not Db_Conn.Success then
         Panic (Error_Sql_Query, Db_Conn.Error);
      end if;

      while Result.Has_Row loop
         Root_Folder_Title := To_Unbounded_String (Result.Value (0));
         Result.Next;
      end loop;

      case Result.Processed_Rows is
         when 0 =>
            Panic (Error_Folder_Not_Found);

         when 1 =>
            Put_Line
              (To_String (Marker_Folder_Pre) & To_String (Root_Folder_Title));
            Recursive_Image (Root_Folder_Id, 1);
            Put_Line (To_String (Marker_Folder_Post));

         when others =>
            Panic (Error_Folder_Not_Unique);
      end case;
   end Image;

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

      package String_Sets is new
        Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);

      Objects : String_Sets.Set;
      Folders : String_Sets.Set;

      type Element_Type is (Type_Object, Type_Folder);
      for Element_Type use (Type_Object => 1, Type_Folder => 2);
   begin
      if Current_Depth > Recursion_Depth then
         Panic
           (Error_Max_Tree_Depth, "Current value: " & Recursion_Depth'Image);
      end if;

      Result.Fetch (Db_Conn, Query);

      if not Db_Conn.Success then
         Panic (Error_Sql_Query, Db_Conn.Error);
      end if;

      while Result.Has_Row loop
         case Result.Integer_Value (1) is
            when Type_Object'Enum_Rep =>
               if not Allow_Doubles then
                  begin
                     Objects.Insert (To_Unbounded_String (Result.Value (4)));
                  exception
                     when Constraint_Error =>
                        Panic
                          (Error_Doubled_Elements,
                           "Value: " & Result.Value (4));
                  end;
               end if;

               Put_Line
                 (To_String (Marker_Object_Pre)
                  & Result.Value (4)
                  & To_String (Marker_Object_Post));

            when Type_Folder'Enum_Rep =>
               if not Allow_Doubles then
                  begin
                     Folders.Insert (To_Unbounded_String (Result.Value (3)));
                  exception
                     when Constraint_Error =>
                        Panic
                          (Error_Doubled_Elements,
                           "Value: " & Result.Value (3));
                  end;
               end if;

               Put_Line (To_String (Marker_Folder_Pre) & Result.Value (3));
               Recursive_Image (Result.Integer_Value (0), Current_Depth + 1);
               Put_Line (To_String (Marker_Folder_Post));

            when others =>
               Panic
                 (Error_Unknown_Type,
                  "Type: " & Result.Integer_Value (1)'Image);
         end case;

         Result.Next;
      end loop;
   end Recursive_Image;

   procedure Clean_Up is
   begin
      Free (Db_Conn);
      Free (Db_Descr);
   end Clean_Up;

end Imaging;
