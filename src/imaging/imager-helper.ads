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
