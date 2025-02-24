-- Auto generated with gnatcoll_db2ada.

with GNATCOLL.SQL; use GNATCOLL.SQL;
package Database_Names is
   pragma Style_Checks (Off);
   TC_Moz_Bookmarks : aliased constant String := "moz_bookmarks";
   Ta_Moz_Bookmarks : constant Cst_String_Access := TC_Moz_Bookmarks'Access;
   TC_Moz_Places : aliased constant String := "moz_places";
   Ta_Moz_Places : constant Cst_String_Access := TC_Moz_Places'Access;

   NC_Fk : aliased constant String := "fk";
   N_Fk : constant Cst_String_Access := NC_fk'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Parent : aliased constant String := "parent";
   N_Parent : constant Cst_String_Access := NC_parent'Access;
   NC_Title : aliased constant String := "title";
   N_Title : constant Cst_String_Access := NC_title'Access;
   NC_Type : aliased constant String := """type""";
   N_Type : constant Cst_String_Access := NC_type'Access;
   NC_Url : aliased constant String := "url";
   N_Url : constant Cst_String_Access := NC_url'Access;
end Database_Names;
