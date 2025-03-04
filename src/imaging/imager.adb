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

with Imager.Description; use Imager.Description;
with Imager.Firefox;
with Imager.Firefox.Sqlite;

package body Imager is

   procedure Initialize
     (Database     : Unbounded_String;
      Check_Syntax : Boolean;
      Doubles      : Boolean;
      Tree_Depth   : Positive;
      Folder_Pre   : Unbounded_String;
      Folder_Post  : Unbounded_String;
      Object_Pre   : Unbounded_String;
      Object_Post  : Unbounded_String) is
   begin
      Img_Descr :=
        (Database,
         Check_Syntax,
         Doubles,
         Tree_Depth,
         Folder_Pre,
         Folder_Post,
         Object_Pre,
         Object_Post);

      Imager.Firefox.Sqlite.Initialize;

      Is_Initialized := True;
   end Initialize;

   procedure Image (Root_Folder_Title : String) is
   begin
      Imager.Firefox.Sqlite.Image (Root_Folder_Title);
   end Image;

   procedure Image (Root_Folder_Id : Natural) is
   begin
      Imager.Firefox.Sqlite.Image (Root_Folder_Id);
   end Image;

   procedure Clean_Up is
   begin
      Imager.Firefox.Sqlite.Clean_Up;
      Is_Initialized := False;
   end Clean_Up;

end Imager;
