-----------------------------------------------------------------------
--  Marshaller -- Marshalling of data in communication buffer
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
with System;
with MAT.Types;
package MAT.Readers.Marshaller is

   Buffer_Underflow_Error : exception;

   Buffer_Overflow_Error : exception;

   function Get_Raw_Uint32 (Buf : System.Address) return MAT.Types.Uint32;

   function Get_Uint8 (Buffer : in Buffer_Ptr) return MAT.Types.Uint8;

   function Get_Uint16 (Buffer : in Buffer_Ptr) return MAT.Types.Uint16;

   function Get_Uint32 (Buffer : in Buffer_Ptr) return MAT.Types.Uint32;

   function Get_Uint64 (Buffer : in Buffer_Ptr) return MAT.Types.Uint64;

   --  procedure Put_Uint8 (Buffer : in Buffer_Ptr; Data : in Uint8);

   --  procedure Put_Uint16 (Buffer : in Buffer_Ptr; Data : in Uint16);

   --  procedure Put_Uint32 (Buffer : in Buffer_Ptr; Data : in Uint32);

end MAT.Readers.Marshaller;
