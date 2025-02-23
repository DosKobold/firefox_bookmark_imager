with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

package body Error_Handling is

   procedure Panic (Message : String) is
   begin
         Put_Line (Current_Error, "ERROR: " & Message);
         OS_Exit (1);
   end Panic;

end Error_Handling;