-----------------------------------------------------------------------
--  mat-consoles - Console interface
--  Copyright (C) 2014, 2015, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with MAT.Types;
package MAT.Consoles is

   type Field_Type is (F_ADDR,
                       F_SIZE,
                       F_TOTAL_SIZE,
                       F_MIN_SIZE,
                       F_MAX_SIZE,
                       F_MIN_ADDR,
                       F_MAX_ADDR,
                       F_THREAD,
                       F_COUNT,
                       F_LEVEL,
                       F_FILE_NAME,
                       F_FUNCTION_NAME,
                       F_LINE_NUMBER,
                       F_START_TIME,
                       F_END_TIME,
                       F_MALLOC_COUNT,
                       F_REALLOC_COUNT,
                       F_FREE_COUNT,
                       F_EVENT_RANGE,
                       F_ID,
                       F_OLD_ADDR,
                       F_GROW_SIZE,
                       F_TIME,
                       F_DURATION,
                       F_EVENT,
                       F_NEXT,
                       F_PREVIOUS,
                       F_MODE,
                       F_FRAME_ID,
                       F_RANGE_ADDR,
                       F_FRAME_ADDR);

   type Notice_Type is (N_HELP,
                        N_INFO,
                        N_EVENT_ID,
                        N_PID_INFO,
                        N_DURATION,
                        N_PATH_INFO);

   type Justify_Type is (J_LEFT,           --  Justify left   |item    |
                         J_RIGHT,          --  Justify right  |    item|
                         J_CENTER,         --  Justify center |  item  |
                         J_RIGHT_NO_FILL   --  Justify right  |item|
                        );

   type Console_Type is abstract tagged limited private;
   type Console_Access is access all Console_Type'Class;

   --  Report an error message.
   procedure Error (Console : in out Console_Type;
                    Message : in String) is abstract;

   --  Report a notice message.
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in String) is abstract;

   --  Print the field value for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String;
                          Justify : in Justify_Type := J_LEFT) is abstract;

   --  Print the title for the given field.
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is abstract;

   --  Start a new title in a report.
   procedure Start_Title (Console : in out Console_Type) is abstract;

   --  Finish a new title in a report.
   procedure End_Title (Console : in out Console_Type) is abstract;

   --  Start a new row in a report.
   procedure Start_Row (Console : in out Console_Type) is abstract;

   --  Finish a new row in a report.
   procedure End_Row (Console : in out Console_Type) is abstract;

   --  Print the title for the given field and setup the associated field size.
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String;
                          Length  : in Positive);

   --  Format the address and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Addr    : in MAT.Types.Target_Addr);

   --  Format the address range and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          From    : in MAT.Types.Target_Addr;
                          To      : in MAT.Types.Target_Addr);

   --  Format the size and print it for the given field.
   procedure Print_Size (Console : in out Console_Type;
                         Field   : in Field_Type;
                         Size    : in MAT.Types.Target_Size);

   --  Format the thread information and print it for the given field.
   procedure Print_Thread (Console : in out Console_Type;
                           Field   : in Field_Type;
                           Thread  : in MAT.Types.Target_Thread_Ref);

   --  Format the time tick as a duration and print it for the given field.
   procedure Print_Duration (Console  : in out Console_Type;
                             Field    : in Field_Type;
                             Duration : in MAT.Types.Target_Tick_Ref);

   --  Format the integer and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Integer;
                          Justify : in Justify_Type := J_LEFT);

   --  Format the integer and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Ada.Strings.Unbounded.Unbounded_String;
                          Justify : in Justify_Type := J_LEFT);

   --  Get the field count that was setup through the Print_Title calls.
   function Get_Field_Count (Console : in Console_Type) return Natural;

   --  Reset the field count.
   procedure Clear_Fields (Console : in out Console_Type);

   --  Enable or disable the use of colors for the outputs.
   procedure Set_Color (Console : in out Console_Type;
                        Enable  : in Boolean);

private

   type Field_Size_Array is array (Field_Type) of Natural;

   type Field_List_Array is array (1 .. Field_Type'Pos (Field_Type'Last)) of Field_Type;

   type Console_Type is abstract tagged limited record
      Sizes       : Field_Size_Array := (others => 1);
      Cols        : Field_Size_Array := (others => 1);
      Fields      : Field_List_Array;
      Field_Count : Natural := 0;
      Use_Colors  : Boolean := False;
   end record;

end MAT.Consoles;
