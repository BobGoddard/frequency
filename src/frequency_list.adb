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

with Ada.Containers; use Ada.Containers;

package body Frequency_List is

   function  DeQueue_Data return Frequency_Record.Frequency_List_Records_Type is
      Data : Frequency_Record.Frequency_List_Records_Type;
   begin
      Data_Queue.Dequeue (Data);
      return Data;
   end DeQueue_Data;

   function Is_Data_Available return Boolean is
   begin
      if Data_Queue.Current_Use > 0 then
         return True;
      end if;
      return False;
   end Is_Data_Available;

   procedure Queue_Data (Data : Frequency_Record.Frequency_List_Records_Type) is
   begin
      Data_Queue.Enqueue (Data);
   end Queue_Data;
end Frequency_List;
