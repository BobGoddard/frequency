--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO;
with Input_Sources;
with Input_Sources.File;
with Sax.Readers;
with DOM.Readers; use DOM.Readers;
with DOM.Core;
with DOM.Core.Attrs;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DB_Routines;

package body Config_Handler is
   Doc   : DOM.Core.Document;

   procedure Load_Config is
      Input_File : Input_Sources.File.File_Input;
      Reader     : DOM.Readers.Tree_Reader;
      List       : DOM.Core.Node_List;
      N          : DOM.Core.Node;
      A          : DOM.Core.Attr;
      Host       : Ada.Strings.Unbounded.Unbounded_String;
      DB         : Ada.Strings.Unbounded.Unbounded_String;
      User       : Ada.Strings.Unbounded.Unbounded_String;
      Pass       : Ada.Strings.Unbounded.Unbounded_String;
      Port       : Integer := 3306;
   begin
      Input_Sources.File.Set_Public_Id (Input_File, "Configuration file");
      Input_Sources.File.Open (Ada.Strings.Unbounded.To_String (Config_Name), Input_File);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);
      Set_Feature (Reader, Sax.Readers.Namespace_Feature, False);
      DOM.Readers.Parse (Reader, Input_File);
      Input_Sources.File.Close (Input_File);
      Doc := DOM.Readers.Get_Tree (Reader);
      List := DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "Database");

      for Index in 1 .. DOM.Core.Nodes.Length (List) loop
         N := DOM.Core.Nodes.Item (List, Index - 1);
         A := DOM.Core.Nodes.Get_Named_Item (DOM.Core.Nodes.Attributes (N), "name");
         Ada.Text_IO.Put ("Value of '");
         Ada.Text_IO.Put (DOM.Core.Attrs.Value (A));
         Ada.Text_IO.Put ("' is ");
         Ada.Text_IO.Put (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N)));
         Ada.Text_IO.New_Line;

         if DOM.Core.Attrs.Value (A) = "host" then
            Ada.Text_IO.Put_Line ("Setting host");
            Host := Ada.Strings.Unbounded.To_Unbounded_String (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N)));
         elsif DOM.Core.Attrs.Value (A) = "db" then
            Ada.Text_IO.Put_Line ("Setting database");
            DB := Ada.Strings.Unbounded.To_Unbounded_String (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N)));
         elsif DOM.Core.Attrs.Value (A) = "user" then
            Ada.Text_IO.Put_Line ("Setting user");
            User := Ada.Strings.Unbounded.To_Unbounded_String (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N)));
         elsif DOM.Core.Attrs.Value (A) = "pass" then
            Ada.Text_IO.Put_Line ("Setting pass");
            Pass := Ada.Strings.Unbounded.To_Unbounded_String (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N)));
         elsif DOM.Core.Attrs.Value (A) = "port" then
            Ada.Text_IO.Put_Line ("Setting port");
            Port := Integer'Value (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N)));
         end if;
      end loop;

      DOM.Core.Free (List);
      DOM.Readers.Free (Reader);
      DB_Routines.Set_Account_Details (Host, DB, User, Pass, Port);
   end Load_Config;

   procedure Save_Config is
   begin
      null;
   end Save_Config;

   procedure Set_Config_Name (S : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Config_Name := S;
   end Set_Config_Name;
end Config_Handler;
