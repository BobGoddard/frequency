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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Frequency_Record;
with Interfaces;

package Frequency_List is

   type Frequency_Count_Rec is record
      Time_Fired : Interfaces.Unsigned_64;
      Counter    : Long_Integer;
   end record;

   package Data_Map_Queue_Interface       is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => Frequency_Record.Frequency_List_Records_Type);
   package Data_Map_Queue                 is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Data_Map_Queue_Interface);

   Data_Queue       : Data_Map_Queue.Queue;

   function  DeQueue_Data      return Frequency_Record.Frequency_List_Records_Type;
   function  Is_Data_Available return Boolean;
   procedure Queue_Data        (Data : Frequency_Record.Frequency_List_Records_Type);
end Frequency_List;
