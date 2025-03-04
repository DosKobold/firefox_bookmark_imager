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

with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;

package Imager.Firefox.Sqlite is

   --  Inititalizes the Imager
   procedure Initialize
   with Post => Is_Initialized;

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

   Db_Conn  : Database_Connection;
   Db_Descr : Database_Description;

   --  Start imaging when there is one found folder. Else panic
   procedure Image_Root_Folder
     (Found_Folders : Natural; Id : Natural; Title : String);

   --  Goes through tree till maximum depth is reached
   procedure Recursive_Image (Root_Id : Natural; Current_Depth : Positive)
   with Pre => Is_Initialized;

   --  Checks if the query was successfull and panics if needed
   procedure Check_Query_Success;

   --  Prints the Folder and it's content
   procedure Print_Folder (Id : Natural; Name : String; Depth : Positive);

   --  Checks if the imager is initialized
   function Is_Initialized return Boolean
   is (Db_Conn /= null);

end Imager.Firefox.Sqlite;
