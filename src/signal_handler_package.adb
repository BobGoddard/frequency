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

with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;

package body Signal_Handler_Package is
  protected body Signal_Handler is
      procedure Handle_Control_C is
      begin
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Control C raised - " & GNAT.Source_Info.Source_Location);
         Exit_Program := True;
      end Handle_Control_C;

      procedure Handle_Term is
      begin
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "SigTerm raised - " & GNAT.Source_Info.Source_Location);
         Exit_Program := True;
      end Handle_Term;

      function Is_Exit_Called return Boolean is
      begin
         return Exit_Program;
      end Is_Exit_Called;

      procedure Raise_Exit is
      begin
         Exit_Program := True;
      end Raise_Exit;

      procedure Setup is
      begin
         pragma Unreserve_All_Interrupts;
      end Setup;
  end Signal_Handler;
end Signal_Handler_Package;
