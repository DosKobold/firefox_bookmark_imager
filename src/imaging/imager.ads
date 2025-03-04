with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Imager is

   --  Inititalizes the Imager
   procedure Initialize
     (Database     : Unbounded_String;
      Check_Syntax : Boolean;
      Doubles      : Boolean;
      Tree_Depth   : Positive;
      Folder_Pre   : Unbounded_String;
      Folder_Post  : Unbounded_String;
      Object_Pre   : Unbounded_String;
      Object_Post  : Unbounded_String)
   with Pre => Length (Database) > 0, Post => Is_Initialized;

   --  Images the folder onto stdout with the initialized database
   procedure Image (Root_Folder_Title : String)
   with Pre => Is_Initialized;

   --  Images the folder onto stdout with the initialized database
   procedure Image (Root_Folder_Id : Natural)
   with Pre => Is_Initialized;

   --  Close connection and free memory
   procedure Clean_Up
   with Pre => Is_Initialized, Post => not Is_Initialized;

   Is_Initialized : Boolean := False;

end Imager;
