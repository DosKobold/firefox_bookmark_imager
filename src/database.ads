--  Auto generated with gnatcoll_db2ada.
--  Manual change: Change name of "Type" into "Type_Value"

with GNATCOLL.SQL; use GNATCOLL.SQL;
pragma Warnings (Off, "no entities of * are referenced");
pragma Warnings (Off, "use clause for package * has no effect");
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
pragma Warnings (On, "no entities of * are referenced");
pragma Warnings (On, "use clause for package * has no effect");
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Moz_Bookmarks
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Moz_Bookmarks, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Moz_Bookmarks, Instance, N_Id, Index);
      Type_Value : SQL_Field_Integer (Ta_Moz_Bookmarks, Instance, N_Type, Index);
      Fk : SQL_Field_Integer (Ta_Moz_Bookmarks, Instance, N_Fk, Index);
      Parent : SQL_Field_Integer (Ta_Moz_Bookmarks, Instance, N_Parent, Index);
      Title : SQL_Field_Text (Ta_Moz_Bookmarks, Instance, N_Title, Index);
   end record;

   type T_Moz_Bookmarks (Instance : Cst_String_Access)
      is new T_Abstract_Moz_Bookmarks (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Moz_Bookmarks (Index : Integer)
      is new T_Abstract_Moz_Bookmarks (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Moz_Places
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Moz_Places, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Moz_Places, Instance, N_Id, Index);
      Url : SQL_Field_Integer (Ta_Moz_Places, Instance, N_Url, Index);
   end record;

   type T_Moz_Places (Instance : Cst_String_Access)
      is new T_Abstract_Moz_Places (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Moz_Places (Index : Integer)
      is new T_Abstract_Moz_Places (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   Moz_Bookmarks : T_Moz_Bookmarks (null);
   Moz_Places : T_Moz_Places (null);
end Database;
