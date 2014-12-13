-----------------------------------------------------------------------
--  mat-readers-streams -- Reader for streams
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
with Util.Streams.Buffered;
with MAT.Events.Probes;
with MAT.Events.Targets;
package MAT.Readers.Streams is

   type Stream_Reader_Type is new MAT.Events.Probes.Probe_Manager_Type with private;

   --  Read the events from the stream and stop when the end of the stream is reached.
   procedure Read_All (Reader : in out Stream_Reader_Type);

   --  Read a message from the stream.
   overriding
   procedure Read_Message (Reader : in out Stream_Reader_Type;
                           Msg    : in out Message_Type);

private

   type Stream_Reader_Type is new MAT.Events.Probes.Probe_Manager_Type with record
      Stream : Util.Streams.Buffered.Buffered_Stream;
      Data   : Util.Streams.Buffered.Buffer_Access;
   end record;

end MAT.Readers.Streams;
