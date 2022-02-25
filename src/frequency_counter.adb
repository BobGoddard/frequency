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
with Ada.Float_Text_IO;
--  with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Traces.Syslog;
with Linux_GPIO;

package body Frequency_Counter is
   GPIO_Pin         : constant Linux_GPIO.Pin_Num := 4;
   FD               : Linux_GPIO.FD_Type;
   GPIO_Chip_Device : constant String := "/dev/gpiochip0";
   Event_Data       : aliased Linux_GPIO.GPIO_Event_Data;
   Consumer_Name    : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("Frequency counter");

   function Get_Counter return Long_Integer is
   begin
      return Counter;
   end Get_Counter;

   procedure Get_Frequency (Frequency, Access_Time : out Float; Excess_Frequencies : out Local_Defs.Excess_Level) is
      Frequency_Count  : Long_Integer := 1;
      Time_Fired_Start : Interfaces.Unsigned_64;
      Time_Fired_End   : Interfaces.Unsigned_64;
   begin

      if not Running then
         raise Program_Error;
      end if;

      Ada.Float_Text_IO.Default_Fore := 1;
      Ada.Float_Text_IO.Default_Aft  := 3;
      Ada.Float_Text_IO.Default_Exp  := 0;

--      for i in 1 .. 2 loop
      Frequency_Count := 0;
      Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);
      Time_Fired_Start    := Event_Data.Timestamp;

      duration_1 :
      loop
         Frequency_Count := Frequency_Count + 1;
         Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);
         Frequency := Float (Event_Data.Timestamp rem 10_000_000_000_000) / 1_000_000_000.0;
         if Frequency_Count = 20 then
            exit duration_1;
         end if;
      end loop duration_1;
      --      end loop;
      Frequency_Count := 0;
      Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);
      Time_Fired_Start    := Event_Data.Timestamp;

      duration_2      :
      loop
         Frequency_Count := Frequency_Count + 1;
         Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);
         Frequency := Float (Event_Data.Timestamp rem 10_000_000_000_000) / 1_000_000_000.0;
         if Frequency_Count = 20 then
            exit duration_2;
         end if;
      end loop duration_2;

      Time_Fired_End := Event_Data.Timestamp;
      Frequency      := Float (Frequency_Count) * 1_000_000_000.0 / Float (Time_Fired_End - Time_Fired_Start);
--      Ada.Text_IO.Put ("Frequency_Count : " & Frequency_Count'Image &
--                         ", Initial_Count: " & Counter'Image &
--                         ", Start: " & Time_Fired_Start'Image &
--                         ", End: " & Time_Fired_End'Image &
--                         ", diff: " & Interfaces.Unsigned_64'Image (Time_Fired_End - Time_Fired_Start) & ", ");

      if Frequency > 50.5 or else Frequency < 49.5 then
--         Ada.Text_IO.Put ("Excess : ");
         Excess_Frequencies := Local_Defs.Excess;
      elsif Frequency > 50.2 or else Frequency < 49.8 then
--         Ada.Text_IO.Put ("Nearing : ");
         Excess_Frequencies := Local_Defs.Nearing;
      else
--         Ada.Text_IO.Put ("Normal : ");
         Excess_Frequencies := Local_Defs.Normal;
      end if;

--      Ada.Float_Text_IO.Put (Frequency);
--      Ada.Text_IO.New_Line;

      if Excess_Frequencies /= Local_Defs.Normal then
         Frequency_Count    := 0;
         while Frequency_Count <= Counter loop
            Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);
            Frequency_Count := Frequency_Count + 1;
         end loop;
      end if;

      Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);
      Time_Fired_Start := Event_Data.Timestamp;
      Frequency_Count := 0;

      get_duration :
      loop
         Frequency_Count := Frequency_Count + 1;
         Linux_GPIO.Monitor_Wait_For_Signal (FD, Event_Data);

         if Frequency_Count >= Counter then
            exit get_duration;
         end if;
      end loop get_duration;

      Time_Fired_End := Event_Data.Timestamp;
      Frequency      := Float (Frequency_Count) * 1_000_000_000.0 / Float (Time_Fired_End - Time_Fired_Start);
      Access_Time    := Float (Time_Fired_End - Time_Fired_Start) / 1_000_000_000.0;

   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         raise;
   end Get_Frequency;

   procedure Initialize is
   begin
      Linux_GPIO.Monitor_Device_Event_Open (GPIO_Chip_Device, GPIO_Pin, Linux_GPIO.GPIOEVENT_REQUEST_FALLING_EDGE, Consumer_Name, FD);
      Running := True;
   end Initialize;

   procedure Set_Counter (New_Counter : Long_Integer) is
   begin
      Counter := New_Counter;
   end Set_Counter;
end Frequency_Counter;
