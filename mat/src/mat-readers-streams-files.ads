-----------------------------------------------------------------------
--  mat-readers-files -- Reader for files
--  Copyright (C) 2014, 2023 Stephane Carrez
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
with Util.Streams.Files;
package MAT.Readers.Streams.Files is

   type File_Reader_Type is new MAT.Readers.Streams.Stream_Reader_Type with private;

   --  Open the file.
   procedure Open (Reader : in out File_Reader_Type;
                   Path   : in String);

private

   type File_Reader_Type is new MAT.Readers.Streams.Stream_Reader_Type with record
      File   : aliased Util.Streams.Files.File_Stream;
   end record;

end MAT.Readers.Streams.Files;
