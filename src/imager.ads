with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;

package Imager is

   type Image_Description is record
      Database    : Unbounded_String;
      Doubles     : Boolean;
      Tree_Depth  : Positive;
      Folder_Pre  : Unbounded_String;
      Folder_Post : Unbounded_String;
      Object_Pre  : Unbounded_String;
      Object_Post : Unbounded_String;
   end record;

   --  Inititalizes the Imager
   procedure Initialize (Given_Descr : Image_Description)
   with Pre => Length (Given_Descr.Database) > 0, Post => Is_Initialized;

   --  Images the folder onto stdout with the initialized database
   procedure Image (Root_Folder_Title : String)
   with Pre => Is_Initialized;

   --  Images the folder onto stdout with the initialized database
   procedure Image (Root_Folder_Id : Natural)
   with Pre => Is_Initialized;

   --  Close connection and free memory
   procedure Clean_Up
   with Pre => Is_Initialized, Post => not Is_Initialized;

   function Is_Initialized return Boolean
   with Ghost;

private

   Img_Descr   : Image_Description;
   Db_Conn     : Database_Connection;
   Db_Descr    : Database_Description;

   Type_Object : constant Positive := 1;
   Type_Folder : constant Positive := 2;

   package String_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);

   function Is_Initialized return Boolean
   is (Db_Conn /= null);

   --  Start imaging when there is one found folder. Else panic
   procedure Image_Root_Folder
     (Found_Folders : Natural; Id : Natural; Title : String);

   --  Goes through tree till maximum depth is reached
   procedure Recursive_Image (Root_Id : Natural; Current_Depth : Positive)
   with Pre => Is_Initialized;

   --  Checks if the query was successfull and panics if needed
   procedure Check_Query_Success;

   --  Appends the string to the set and panics if needed
   procedure Check_For_Doubles
     (Elements : in out String_Sets.Set; Content : String);

   --  Prints the Folder and it's content
   procedure Print_Folder (Id : Natural; Name : String; Depth : Positive);

   --  Prints the object
   procedure Print_Object (Content : String);

end Imager;
