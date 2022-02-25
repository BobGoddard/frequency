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

with GNAT.Traceback;
with Local_Defs; use Local_Defs;

package Frequency_Counter is
   function  Get_Counter return Long_Integer;
   procedure Get_Frequency (Frequency, Access_Time : out Float; Excess_Frequencies : out Local_Defs.Excess_Level);
   procedure Initialize;
   procedure Set_Counter (New_Counter : Long_Integer);

private
   Counter : Long_Integer := 100;
   Running : Boolean      := False;
   Trace   : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length  : Natural;
end Frequency_Counter;
