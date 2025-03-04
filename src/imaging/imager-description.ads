private package Imager.Description is

   type Image_Description is record
      Database     : Unbounded_String;
      Check_Syntax : Boolean;
      Doubles      : Boolean;
      Tree_Depth   : Positive;
      Folder_Pre   : Unbounded_String;
      Folder_Post  : Unbounded_String;
      Object_Pre   : Unbounded_String;
      Object_Post  : Unbounded_String;
   end record;

   Img_Descr : Image_Description;

end Imager.Description;
