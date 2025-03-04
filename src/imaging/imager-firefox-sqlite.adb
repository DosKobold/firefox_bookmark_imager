--  Copyright (c) 2025, Paul Stöckle <paul.stoeckle@t-online.de>
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--
--  3. Neither the name of the copyright holder nor the names of its
--     contributors may be used to endorse or promote products derived from
--     this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.

with Database;              use Database;
with Error_Handling;        use Error_Handling;
with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Sqlite;
with Imager.Description;    use Imager.Description;
with Imager.Helper;         use Imager.Helper;

package body Imager.Firefox.Sqlite is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Db_Descr := GNATCOLL.SQL.Sqlite.Setup (To_String (Img_Descr.Database));
      Db_Conn := Db_Descr.Build_Connection;
   end Initialize;

   -----------
   -- Image --
   -----------

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

   -----------
   -- Image --
   -----------

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

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
   begin
      Free (Db_Conn);
      Free (Db_Descr);
   end Clean_Up;

   ----------------------------------------------------------------------------

   -----------------------
   -- Image_Root_Folder --
   -----------------------

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

   ---------------------
   -- Recursive_Image --
   ---------------------

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

               Print_Folder_Pre (Result.Value (3));
               Recursive_Image (Result.Integer_Value (0), Current_Depth);
               Print_Folder_Post;

            when others =>
               Panic
                 (Error_Unknown_Type,
                  "Type: " & Result.Integer_Value (1)'Image);
         end case;

         Result.Next;
      end loop;
   end Recursive_Image;

   -------------------------
   -- Check_Query_Success --
   -------------------------

   procedure Check_Query_Success is
   begin
      if not Db_Conn.Success then
         Panic (Error_Sql_Query, Db_Conn.Error);
      end if;
   end Check_Query_Success;

   ------------------
   -- Print_Folder --
   ------------------

   procedure Print_Folder (Id : Natural; Name : String; Depth : Positive) is
   begin
      Print_Folder_Pre (Name);
      Recursive_Image (Id, Depth);
      Print_Folder_Post;
   end Print_Folder;

end Imager.Firefox.Sqlite;
