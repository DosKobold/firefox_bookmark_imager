with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed;

private package Imager.Helper is

   package String_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);

   --  Appends the string to the set and panics if needed
   procedure Check_For_Doubles
     (Elements : in out String_Sets.Set; Content : String);

   --  Prints the first part and checks syntax if needed
   procedure Print_Folder_Pre (Name : String);

   --  Prints the second part
   procedure Print_Folder_Post;

   --  Prints the object and checks syntax if needed
   procedure Print_Object (Content : String);

private

   --  Checks if folder contains pre or post markings and panics if needed
   procedure Check_Folder_Syntax (Name : String);

   --  Checks if object contains pre or post markings and panics if needed
   procedure Check_Object_Syntax (Name : String);

   --  Checks if the source string contains the pattern
   function Contains
     (Source : String; Pattern : Unbounded_String) return Boolean
   is (Ada.Strings.Fixed.Count (Source, To_String (Pattern)) > 0);

end Imager.Helper;
