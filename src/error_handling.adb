with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Error_Handling is

   procedure Panic (Class : Error; Message : String := "") is
   begin
      if Message'Length > 0 then
         Put_Line (Current_Error, Message);
      end if;

      Put_Line
        (Current_Error,
         To_String ("ERROR(" & Class.Id'Image & "): " & Class.Description));

      if Length (Class.Hints) > 0 then
         Put_Line (Current_Error, To_String ("   " & Class.Hints));
      end if;

      OS_Exit (Class.Id);
   end Panic;

end Error_Handling;
