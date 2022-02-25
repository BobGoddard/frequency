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
with GNATCOLL.Traces;
with GNATCOLL.Traces.Syslog;
with Frequency_List;
with Frequency_Record;
with DB_Routines;
with Local_Defs; use Local_Defs;
with Signal_Handler_Package;

package body Frequency_DB_Update is
   function Process_Data return Local_Defs.Trilean is
      Data    : Frequency_Record.Frequency_List_Records_Type;
      Result  : Local_Defs.Trilean;
   begin
      if Frequency_List.Is_Data_Available then
         Data := Frequency_List.DeQueue_Data;
         Result := DB_Routines.DB_Is_TS_Base (Data.Base_Time);

         if Result = TBroken then
            DB_Routines.DB_Disconnect;
            Result := DB_Routines.DB_Connect;

            if Result = TBroken then
               Signal_Handler_Package.Signal_Handler.Raise_Exit;
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Frequency DB Update " & GNAT.Source_Info.Source_Location);
               return Result;
            end if;
         end if;

         if Result = TFalse then
            Result := DB_Routines.DB_Insert_TS_Base (Data.Base_Time);
            if Result = TFalse then
               DB_Routines.DB_Disconnect;
               Result := DB_Routines.DB_Connect;
               if Result = TFalse or else Result = TBroken then
                  Signal_Handler_Package.Signal_Handler.Raise_Exit;
                  GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Frequency DB Update " & GNAT.Source_Info.Source_Location);
                  return Result;
               end if;
            end if;
         end if;

         Result := DB_Routines.DB_Update_TS (Data);

         if Result = TFalse or else Result = TBroken then
            Signal_Handler_Package.Signal_Handler.Raise_Exit;
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Frequency DB Update " & GNAT.Source_Info.Source_Location);
            return Result;
         end if;
      end if;

      return Local_Defs.TTrue;
   end Process_Data;

   task body Update is
      Do_Exit : Boolean := False;
      Result  : Local_Defs.Trilean;
   begin
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Frequency db update task started." & GNAT.Source_Info.Source_Location);
      select
         accept Start;
      end select;

      busy : loop
         select
            accept Kill_Me do
               Do_Exit := True;
            end Kill_Me;
         else
            Result := Process_Data;
            if Result /= Local_Defs.TTrue then
               exit busy;
            end if;
         end select;

         if Do_Exit then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Frequency DB Update " & GNAT.Source_Info.Source_Location);
            exit busy;
         end if;

         delay 0.9;
      end loop busy;

      if Result = Local_Defs.TTrue then
         Result := Process_Data;
         if Result /= Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Frequency DB Update " & GNAT.Source_Info.Source_Location);
         end if;
      end if;

   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         raise;
   end Update;
end Frequency_DB_Update;
