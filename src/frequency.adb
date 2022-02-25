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

with Ada.Exceptions;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with Frequency_Counter;
with Frequency_DB_Update;
with Frequency_Period;
with DB_Routines;
with GNATCOLL.Traces;
with GNATCOLL.Traces.Syslog;
with Local_Defs; use Local_Defs;
with Signal_Handler_Package;
with Ada.Directories;          use Ada.Directories;
with Ada.Strings.Unbounded;
with GNAT.Command_Line;
with Config_Handler;

procedure Frequency is
   type Freq_Period_Type    is new Frequency_Period.Period;
   type Freq_DB_Update_Type is new Frequency_DB_Update.Update;
   Freq_Period    : Freq_Period_Type;
   Freq_DB_Update : Freq_DB_Update_Type;
   Result         : Local_Defs.Trilean;
   Trace          : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length         : Natural;
   XML_Settings   : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("/etc/frequency.xml");
   CMD_Args       : exception;
begin
   GNATCOLL.Traces.Syslog.Register_Syslog_Stream;
   GNATCOLL.Traces.Syslog.Openlog (GNAT.Source_Info.File, GNATCOLL.Traces.Syslog.None, GNATCOLL.Traces.Syslog.Daemon);

   loop
      case GNAT.Command_Line.Getopt ("i:") is
         when 'i' =>
            XML_Settings := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Command_Line.Parameter);
         when others =>
            exit;
      end case;
   end loop;

   if not Ada.Directories.Exists (Ada.Strings.Unbounded.To_String (XML_Settings)) then
      raise CMD_Args with "Problem reading " & Ada.Strings.Unbounded.To_String (XML_Settings);
   end if;

   if Ada.Directories.Kind (Ada.Strings.Unbounded.To_String (XML_Settings)) /= Ada.Directories.Ordinary_File then
      raise CMD_Args with "Config file does not appear to be an ordinary file";
   end if;

   Config_Handler.Set_Config_Name (XML_Settings);
   Config_Handler.Load_Config;
   Signal_Handler_Package.Signal_Handler.Setup;
   Result := DB_Routines.DB_Connect;
   Frequency_Counter.Initialize;

   if Result = Local_Defs.TFalse or else Result = Local_Defs.TFalse then
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Failed to connect to database. Exiting...");
      return;
   end if;

   Freq_Period.Start;
   Freq_DB_Update.Start;

   main :
   while not Signal_Handler_Package.Signal_Handler.Is_Exit_Called loop
      if Freq_Period'Terminated then
         exit main;
      end if;
      if Freq_DB_Update'Terminated then
         exit main;
      end if;
      delay 0.1;
   end loop main;

   if not Freq_Period'Terminated then
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Calling fperiod killme");
      Freq_Period.Kill_Me;
   end if;

   if not Freq_DB_Update'Terminated then
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Calling fdbupdate killme");
      Freq_DB_Update.Kill_Me;
   end if;

exception
   when E : others =>
      GNAT.Traceback.Call_Chain (Trace, Length);
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " &
                                       Ada.Exceptions.Exception_Message (E));
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      raise;
end Frequency;
