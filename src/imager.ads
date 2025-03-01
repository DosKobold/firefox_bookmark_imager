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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;

package Imager is

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

   package String_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);

   Img_Descr : Image_Description;
   Db_Conn   : Database_Connection;
   Db_Descr  : Database_Description;

   Type_Object : constant Positive := 1;
   Type_Folder : constant Positive := 2;

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

   --  Checks if folder contains pre or post markings and panics if needed
   procedure Check_Folder_Syntax (Name : String);

   --  Checks if object contains pre or post markings and panics if needed
   procedure Check_Object_Syntax (Name : String);

   --  Checks if the imager is initialized
   function Is_Initialized return Boolean
   is (Db_Conn /= null);

   --  Checks if the source string contains the pattern
   function Contains
     (Source : String; Pattern : Unbounded_String) return Boolean
   is (Ada.Strings.Fixed.Count (Source, To_String (Pattern)) > 0);

end Imager;
