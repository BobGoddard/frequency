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

with Ada.Interrupts;
with Ada.Interrupts.Names;

package Signal_Handler_Package is
   pragma Unreserve_All_Interrupts;  -- Gnat will no longer handle SIGINT for us
   protected Signal_Handler is
      procedure Handle_Control_C;
      pragma Attach_Handler (Handle_Control_C, Ada.Interrupts.Names.SIGINT);

      procedure Handle_Term;
      pragma Attach_Handler (Handle_Term, Ada.Interrupts.Names.SIGTERM);

      function Is_Exit_Called return Boolean;
      procedure Raise_Exit;
      procedure Setup;
   end Signal_Handler;

private
   Exit_Program : Boolean := False;
end Signal_Handler_Package;
