with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;

package Imaging is

   procedure Initialize (Database : String; Doubles : Boolean := True)
   with Pre => Database'Length > 0, Post => Is_Initialized;

   procedure Image (Root_Folder_Title : String)
   with Pre => Is_Initialized;

   procedure Image (Root_Folder_Id : Natural)
   with Pre => Is_Initialized;

   procedure Clean_Up
   with Pre => Is_Initialized;

   function Is_Initialized return Boolean
   with Ghost;

private

   Db_Conn       : Database_Connection;
   Db_Descr      : Database_Description;
   Allow_Doubles : Boolean;

   function Is_Initialized return Boolean
   is (Db_Conn /= null);

   procedure Recursive_Image (Root_Id : Natural)
   with Pre => Is_Initialized;
end Imaging;
