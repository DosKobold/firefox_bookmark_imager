with Ada.Text_IO;        use Ada.Text_IO;
with Error_Handling;     use Error_Handling;
with Imager.Description; use Imager.Description;

package body Imager.Helper is

   -----------------------
   -- Check_For_Doubles --
   -----------------------

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

   ----------------------
   -- Print_Folder_Pre --
   ----------------------

   procedure Print_Folder_Pre (Name : String) is
   begin
      if Img_Descr.Check_Syntax then
         Check_Folder_Syntax (Name);
      end if;

      Put_Line (To_String (Img_Descr.Folder_Pre) & Name);
      Put_Line (To_String (Img_Descr.Folder_Post));
   end Print_Folder_Pre;

   -----------------------
   -- Print_Folder_Post --
   -----------------------

   procedure Print_Folder_Post is
   begin
      Put_Line (To_String (Img_Descr.Folder_Post));
   end Print_Folder_Post;

   ------------------
   -- Print_Object --
   ------------------

   procedure Print_Object (Content : String) is
   begin
      if Img_Descr.Check_Syntax then
         Check_Object_Syntax (Content);
      end if;

      Put_Line
        (To_String (Img_Descr.Object_Pre)
         & Content
         & To_String (Img_Descr.Object_Post));
   end Print_Object;

   ----------------------------------------------------------------------------

   -------------------------
   -- Check_Folder_Syntax --
   -------------------------

   procedure Check_Folder_Syntax (Name : String) is
   begin
      if (Length (Img_Descr.Folder_Pre) > 0
          and then Contains (Name, Img_Descr.Folder_Pre))
        or else (Length (Img_Descr.Folder_Post) > 0
                 and then Contains (Name, Img_Descr.Folder_Post))
      then
         Panic (Error_Syntax_Check, "Element: " & Name);
      end if;
   end Check_Folder_Syntax;

   -------------------------
   -- Check_Object_Syntax --
   -------------------------

   procedure Check_Object_Syntax (Name : String) is
   begin
      if (Length (Img_Descr.Object_Pre) > 0
          and then Contains (Name, Img_Descr.Object_Pre))
        or else (Length (Img_Descr.Object_Post) > 0
                 and then Contains (Name, Img_Descr.Object_Post))
      then
         Panic (Error_Syntax_Check, "Element: " & Name);
      end if;
   end Check_Object_Syntax;

end Imager.Helper;
