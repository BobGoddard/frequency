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

with AdaBase; use AdaBase;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Maps;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Source_Info;
with GNATCOLL.Traces;
with GNATCOLL.Traces.Syslog;
with Linux_GPIO;

package body DB_Routines is
   DR      : AdaBase.Driver.Base.MySQL.MySQL_Driver;
   Marker1 : Boolean := False;
   Marker2 : Boolean := False;
   Marker3 : Boolean := False;

   function DB_Connect return Local_Defs.Trilean is
      Delay_Time_Max   : constant Duration := 65536.0;
      Delay_Time       : Duration := 1.0;
      Delay_Time_Count : Duration := 0.0;
      Target_Time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      DB_Disconnect;
      delay_loop :
      loop
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Delaying until target time: " & Ada.Calendar.Formatting.Image (Target_Time) & " - " & GNAT.Source_Info.Source_Location);
         delay until Target_Time;

         if Linux_GPIO.Monitor_CTRL_C_Is_Called = True or else (DB_Connect_Private = Local_Defs.TFalse and then Delay_Time_Count >= Delay_Time_Max) then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Failed to make db connection - " & GNAT.Source_Info.Source_Location);
            return Local_Defs.TFalse;
         end if;

         if DB_Connect_Private = Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Connection successful - " & GNAT.Source_Info.Source_Location);
            return Local_Defs.TTrue;
         end if;

         if Delay_Time_Count = Delay_Time then
            Delay_Time := Delay_Time * 2.0;
            Delay_Time_Count := 0.0;
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Delay increased to " & Delay_Time'Image & " - " & GNAT.Source_Info.Source_Location);
         else
            Delay_Time := Delay_Time + 1.0;
         end if;

         Target_Time := Target_Time + Delay_Time;
         Delay_Time_Count := Delay_Time_Count + 1.0;

      end loop delay_loop;
   end DB_Connect;

   function DB_Connect_Private return Local_Defs.Trilean is
   begin
      DR.basic_connect (database => Ada.Strings.Unbounded.To_String (Database),
                        username => Ada.Strings.Unbounded.To_String (DB_User),
                        password => Ada.Strings.Unbounded.To_String (DB_Pass),
                        hostname => Ada.Strings.Unbounded.To_String (DB_Host),
                        port     => DB_Port);
      return Local_Defs.TTrue;

   exception when others => return Local_Defs.TBroken;
   end DB_Connect_Private;

   procedure DB_Disconnect is
   begin
      DR.disconnect;
   end DB_Disconnect;

   function DB_Insert_TS_Base (Base_Time : Interfaces.C.long) return Local_Defs.Trilean is
      Num_Rows : AdaBase.Affected_Rows := 0;
   begin
      declare
         SQL_Insert_TS : constant String := "INSERT INTO frequency (ts) value ("  & Base_Time'Img & ")";
      begin
         Num_Rows := DR.execute (sql => SQL_Insert_TS);
      end;
      DR.commit;

      if Num_Rows /= 1 then
         return Local_Defs.TFalse;
      end if;

      return Local_Defs.TTrue;
   exception when E : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exception raised " & " - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));

         if Marker1 then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Mark1 already raised, Return_Codeuning - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TBroken;
         end if;

         Marker1 := True;

         if DB_Connect = Local_Defs.TFalse then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "DB connect failed, returning TBroken - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TBroken;
         end if;

         if DB_Insert_TS_Base (Base_Time) = Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Install of ts true, returning TTrue - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TTrue;
         end if;

         Marker1 := False;
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Install of ts failed, returning TBroken - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
         return Local_Defs.TBroken;
   end DB_Insert_TS_Base;

   function  DB_Is_TS_Base (Base_Time : Interfaces.C.long) return Local_Defs.Trilean is
      Return_Code : Local_Defs.Trilean := Local_Defs.TTrue;
   begin
      declare
         SQL_TS_Base  : constant String := "SELECT ts FROM frequency where ts=" & Base_Time'Img;
         SQL_STMT         : AdaBase.Statement.Base.MySQL.MySQL_statement := DR.query (SQL_TS_Base);
      begin
         if AdaBase.Statement.Base.MySQL.rows_returned (SQL_STMT) = 0 then
            Return_Code := Local_Defs.TFalse;
         end if;

         SQL_STMT.discard_rest;
         return Return_Code;
      end;

   exception when E : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exception raised " & " - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));

         if Marker2 then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Mark2 already raised, returning - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TBroken;
         end if;

         Marker2 := True;

         if DB_Connect = Local_Defs.TFalse then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "DB connect failed, returning TBroken - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TBroken;
         end if;

         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Return_Coderying db_is_ts_base - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));

         if DB_Is_TS_Base (Base_Time) = Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Is install of ts true, returning TTrue - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TTrue;
         end if;

         Marker2 := False;
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "ts not installed, returning TFalse - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
         return Local_Defs.TFalse;
   end DB_Is_TS_Base;

   function DB_Update_TS (Data : Frequency_Record.Frequency_List_Records_Type) return Local_Defs.Trilean is
      SQL_Query : Ada.Strings.Unbounded.Unbounded_String;
      First     : Boolean := True;
      Avail     : Boolean := False;
      Num_Rows  : AdaBase.Affected_Rows := 0;
   begin
      SQL_Query := Ada.Strings.Unbounded.To_Unbounded_String ("UPDATE frequency set T");

      for SFS in Data.Freq_5m_Record'Range loop
         if Data.Freq_5m_Record (SFS).Valid then
            Avail    := True;

            if First then
               First := False;
            else
               SQL_Query := SQL_Query & ",T";
            end if;

            if SFS <= 9 then
               SQL_Query := SQL_Query & Ada.Strings.Unbounded.To_Unbounded_String ("0");
            end if;

            SQL_Query := SQL_Query & Ada.Strings.Unbounded.Trim (Ada.Strings.Unbounded.To_Unbounded_String (Integer (SFS * 10)'Img), Ada.Strings.Both)
              & Ada.Strings.Unbounded.To_Unbounded_String ("=")
              & Float_To_UnBounded (Data.Freq_5m_Record (SFS).Frequency, 3);
         end if;
      end loop;

      if Avail then
         SQL_Query := SQL_Query & " WHERE ts=" & Ada.Strings.Unbounded.To_Unbounded_String (Data.Base_Time'Img);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "SQL_Query: " & Ada.Strings.Unbounded.To_String (SQL_Query) & " - " & GNAT.Source_Info.Source_Location);
         Num_Rows := DR.execute (sql => To_String (SQL_Query));
         DR.commit;

         if Num_Rows = 1 then
            return Local_Defs.TTrue;
         end if;
      end if;

      return Local_Defs.TBroken;

   exception when E : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exception raised " & " - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));

         if Marker3 then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Mark3 already raised, returning - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TBroken;
         end if;

         Marker3 := True;

         if DB_Connect = Local_Defs.TFalse then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "DB connect failed, returning TBroken - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TBroken;
         end if;

         if DB_Update_TS (Data) = Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Install of ts data true, returning TTrue - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
            return Local_Defs.TTrue;
         end if;

         Marker3 := False;
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Install of ts data false, returning TBroken - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E));
         return Local_Defs.TBroken;
   end DB_Update_TS;

   function Execute_SQL (Data : Ada.Strings.Unbounded.Unbounded_String) return Local_Defs.Trilean is
      Num_Rows    : AdaBase.Affected_Rows := 0;
   begin
      Num_Rows := DR.execute (sql => Ada.Strings.Unbounded.To_String (Data));

      if Num_Rows /= 1 then
         return Local_Defs.TFalse;
      end if;

      return Local_Defs.TTrue;
   end Execute_SQL;

   function Float_To_UnBounded (f : Float; A : Integer) return Ada.Strings.Unbounded.Unbounded_String is
      US : Ada.Strings.Unbounded.Unbounded_String;
      S     : String := "                          ";
      LSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (' ');
      RSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0");
      RSet2 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, f, Aft => A, Exp => 0);
      US := To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return US;
   end Float_To_UnBounded;

   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306) is
   begin
      Database := DB;
      DB_Host  := Host;
      DB_User  := User;
      DB_Pass  := Pass;
      DB_Port  := Port;
   end Set_Account_Details;
end DB_Routines;
