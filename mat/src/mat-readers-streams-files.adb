-----------------------------------------------------------------------
--  mat-readers-files -- Reader for files
--  Copyright (C) 2014 Stephane Carrez
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
with Ada.Streams.Stream_IO;

with Util.Log.Loggers;
with MAT.Readers.Marshaller;
package body MAT.Readers.Streams.Files is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers.Files");

   BUFFER_SIZE  : constant Natural := 100 * 1024;

   --  Open the file.
   procedure Open (Reader : in out File_Reader_Type;
                   Path   : in String) is
   begin
      Log.Info ("Reading data stream {0}", Path);

      Reader.Stream.Initialize (Size   => BUFFER_SIZE,
                                Input  => Reader.File'Unchecked_Access,
                                Output => null);
      Reader.File.Open (Mode => Ada.Streams.Stream_IO.In_File,
                        Name => Path);
   end Open;

end MAT.Readers.Streams.Files;
