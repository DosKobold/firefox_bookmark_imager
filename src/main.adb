--  Copyright (c) 2025, Paul Stöckle <paul.stoeckle@t-online.de>
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--
--  3. Neither the name of the copyright holder nor the names of its
--     contributors may be used to endorse or promote products derived from
--     this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Error_Handling;        use Error_Handling;
with Imager;
with Parse_Args;            use Parse_Args;

procedure Main is
   Arg_Parser : Argument_Parser;

   procedure Initialize_Arg_Parser is
   begin
      Arg_Parser.Add_Option
        (Make_Boolean_Option (False),
         "help",
         'h',
         Usage => "Display this help text");
      Arg_Parser.Add_Option
        (Make_Boolean_Option (False),
         "id-as-root",
         'i',
         Usage => "Use the id for the root folder");
      Arg_Parser.Add_Option
        (Make_Boolean_Option (False),
         "check-syntax",
         'c',
         Usage =>
           "Check syntax for including of pre and post marking elements");
      Arg_Parser.Add_Option
        (Make_Boolean_Option (True),
         "doubles",
         'd',
         Usage => "Disable doubles in the folders");
      Arg_Parser.Add_Option
        (Make_Positive_Option (50),
         "tree-depth",
         't',
         Usage =>
           "Change the tree depth for recursive algorithm. Default: 50");
      Arg_Parser.Add_Option
        (Make_String_Option ("./"),
         "folder-pre",
         Usage => "Change the pre marker of folders");
      Arg_Parser.Add_Option
        (Make_String_Option (""),
         "folder-post",
         Usage => "Change the post marker of folders");
      Arg_Parser.Add_Option
        (Make_String_Option (""),
         "object-pre",
         Usage => "Change the pre marker of objects");
      Arg_Parser.Add_Option
        (Make_String_Option (""),
         "object-post",
         Usage => "Change the post marker of objects");
      Arg_Parser.Append_Positional (Make_String_Option, "DATABASE");
      Arg_Parser.Append_Positional (Make_String_Option, "ROOTFOLDER");
      Arg_Parser.Parse_Command_Line;
   end Initialize_Arg_Parser;
begin
   Initialize_Arg_Parser;

   if not Arg_Parser.Parse_Success then
      Panic (Error_Parsing_Arguments, Arg_Parser.Parse_Message);
   end if;

   if Arg_Parser.Boolean_Value ("help") then
      Arg_Parser.Usage;
      goto Successfull_End_Of_Program;
   end if;

   if Arg_Parser.String_Value ("DATABASE")'Length = 0
     or else Arg_Parser.String_Value ("ROOTFOLDER")'Length = 0
   then
      Panic (Error_Missing_Arguments);
   end if;

   if not Exists (Arg_Parser.String_Value ("DATABASE")) then
      Panic (Error_File_Not_Found);
   end if;

   Imager.Initialize
     (To_Unbounded_String (Arg_Parser.String_Value ("DATABASE")),
      Arg_Parser.Boolean_Value ("check-syntax"),
      Arg_Parser.Boolean_Value ("doubles"),
      Positive (Arg_Parser.Integer_Value ("tree-depth")),
      To_Unbounded_String (Arg_Parser.String_Value ("folder-pre")),
      To_Unbounded_String (Arg_Parser.String_Value ("folder-post")),
      To_Unbounded_String (Arg_Parser.String_Value ("object-pre")),
      To_Unbounded_String (Arg_Parser.String_Value ("object-post")));

   if Arg_Parser.Boolean_Value ("id-as-root") then
      declare
         Folder_Id : Natural;
      begin
         begin
            Folder_Id :=
              Natural'Value (Arg_Parser.String_Value ("ROOTFOLDER"));
         exception
            when Constraint_Error =>
               Panic (Error_Parsing_Root_Id);
         end;

         Imager.Image (Folder_Id);
      end;
   else
      Imager.Image (Arg_Parser.String_Value ("ROOTFOLDER"));
   end if;

   Imager.Clean_Up;

   <<Successfull_End_Of_Program>>
   Set_Exit_Status (0);
end Main;
