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
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Terminal;
with GNATCOLL.Traces.Syslog;
with Frequency_Counter;
with Frequency_List;
with Frequency_Record;
with Local_Defs; use Local_Defs;

package body Frequency_Period is
   task body Period is
      Timeout                :          Ada.Calendar.Time;
      Output_File            :          Ada.Text_IO.File_Type;
      Do_Exit                :          Boolean := False;
      Term_Info              :          GNATCOLL.Terminal.Terminal_Info;
      File_Name              : constant String := "/home/pi/frequency.txt";
      LCount                 :          Interfaces.C.long;
      Frequency              :          Float;
      Access_Time            :          Float;
      FGColour               :          GNATCOLL.Terminal.ANSI_Color;
      BGColour               :          GNATCOLL.Terminal.ANSI_Color;
      Date_Format            : constant GNAT.Calendar.Time_IO.Picture_String := "%c";
      Data                   :          Frequency_Record.Frequency_List_Records_Type;
      Index                  :          Interfaces.C.long;
      Excess_Frequencies     :          Local_Defs.Excess_Level;
   begin
      Ada.Float_Text_IO.Default_Fore := 3;
      Ada.Float_Text_IO.Default_Aft  := 3;
      Ada.Float_Text_IO.Default_Exp  := 0;
      Ada.Integer_Text_IO.Default_Width := 2;

      accept Start;
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Frequency period task started." & GNAT.Source_Info.Source_Location);
      GNATCOLL.Terminal.Init_For_File (Term_Info, Colors => GNATCOLL.Terminal.Yes);

      if GNAT.OS_Lib.Is_Write_Accessible_File (File_Name) then
         Ada.Text_IO.Open (File => Output_File, Mode => Ada.Text_IO.Append_File, Name => File_Name);
      else
         Ada.Text_IO.Create (File => Output_File, Mode => Ada.Text_IO.Out_File, Name => File_Name);
      end if;

      Timeout := Ada.Calendar.Conversions.To_Ada_Time (Ada.Calendar.Conversions.To_Unix_Time (Ada.Calendar.Clock)) + 2.0;
      delay until Timeout;
      Data.Base_Time := Ada.Calendar.Conversions.To_Unix_Time (Timeout) - Ada.Calendar.Conversions.To_Unix_Time (Timeout) rem 300;

      busy : loop
         select
            accept Kill_Me do
               Do_Exit := True;
            end Kill_Me;
         else
            if not Do_Exit then
               if Ada.Calendar.Conversions.To_Unix_Time (Timeout) rem 10 = 0 then
                  Frequency_Counter.Get_Frequency (Frequency, Access_Time, Excess_Frequencies);

                  LCount := Ada.Calendar.Conversions.To_Unix_Time (Timeout) mod 3;

                  if Frequency < 49.5 or else Frequency > 50.5 then
                     FGColour := GNATCOLL.Terminal.Black;
                     BGColour := GNATCOLL.Terminal.Red;
                  elsif Frequency < 49.8 or else Frequency > 50.2 then
                     FGColour := GNATCOLL.Terminal.Yellow;
                     BGColour := GNATCOLL.Terminal.Cyan;
                  else
                     FGColour := GNATCOLL.Terminal.Yellow;
                     BGColour := GNATCOLL.Terminal.Blue;
                  end if;

                  if LCount = 0 then
                     GNATCOLL.Terminal.Set_Color (Term_Info, Term => Output_File, Foreground => FGColour, Background => BGColour, Style => GNATCOLL.Terminal.Dim);
                  elsif LCount = 1 then
                     GNATCOLL.Terminal.Set_Color (Term_Info, Term => Output_File, Foreground => FGColour, Background => BGColour, Style => GNATCOLL.Terminal.Normal);
                  else
                     GNATCOLL.Terminal.Set_Color (Term_Info, Term => Output_File, Foreground => FGColour, Background => BGColour, Style => GNATCOLL.Terminal.Bright);
                  end if;

                  Index := Ada.Calendar.Conversions.To_Unix_Time (Timeout) rem 300 / 10;

                  if Index = 0 then
                     Index := 30;
                  end if;

                  Ada.Text_IO.Put         (Output_File, "Delayed period until " & GNAT.Calendar.Time_IO.Image (Timeout, Date_Format) & "... Access_Time: ");
                  Ada.Float_Text_IO.Put   (Output_File, Access_Time, 3, 9, 0);
                  Ada.Text_IO.Put         (Output_File, ", freq: ");
                  Ada.Float_Text_IO.Put   (Output_File, Frequency);
                  Ada.Text_IO.Put         (Output_File, ", Counters: ");
                  Ada.Integer_Text_IO.Put (Output_File, Integer (Frequency_Counter.Get_Counter), 4);
                  Ada.Text_IO.Put         (Output_File, " ");
                  Ada.Integer_Text_IO.Put (Output_File, Integer (Index));
                  Ada.Text_IO.Put         (Output_File, " - " & Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));

                  if Excess_Frequencies = Local_Defs.Nearing then
                     Ada.Text_IO.Put      (Output_File, "*");
                  elsif Excess_Frequencies = Local_Defs.Excess then
                     Ada.Text_IO.Put      (Output_File, "**");
                  end if;

                  GNATCOLL.Terminal.Set_Color (Term_Info, Term => Output_File, Background => GNATCOLL.Terminal.Reset, Style => GNATCOLL.Terminal.Normal);
                  Ada.Text_IO.New_Line    (Output_File);
                  Ada.Text_IO.Flush       (Output_File);
                  Data.Freq_5m_Record (Integer (Index)).Frequency := Frequency;
                  Data.Freq_5m_Record (Integer (Index)).Valid     := True;

                  if Index = 30 then
                     Frequency_List.Queue_Data (Data);
                     for D in Data.Freq_5m_Record'Range loop
                       Data.Freq_5m_Record (D).Valid := False;
                     end loop;
                     Data.Base_Time := Ada.Calendar.Conversions.To_Unix_Time (Timeout);
                  end if;
               end if;

               Timeout := Timeout + 1.0;
               delay until Timeout;
            end if;
         end select;

         if Do_Exit then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Frequency Period." & GNAT.Source_Info.Source_Location);
            exit busy;
         end if;

      end loop busy;

      Frequency_List.Queue_Data (Data);
      Ada.Text_IO.Close (Output_File);

      if Ada.Text_IO.Is_Open (Output_File) then
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "File did not close: " & GNAT.Source_Info.Source_Location);
      end if;

   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
        raise;
   end Period;
end Frequency_Period;
