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

with Interfaces.C;

package Frequency_Record is
   type Frequency_Record_Type is record
      Frequency : Float := 0.0;
      Valid     : Boolean := False;
   end record;

   type Frequency_Record_Array is array (1 .. 30) of Frequency_Record_Type;

   type Frequency_List_Records_Type is record
      Base_Time      : Interfaces.C.long := 0;
      Freq_5m_Record : Frequency_Record_Array;
   end record;
end Frequency_Record;
