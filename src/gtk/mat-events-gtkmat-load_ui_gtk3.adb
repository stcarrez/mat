-----------------------------------------------------------------------
--  mat-targets-gtkmat - Gtk target management
--  Copyright (C) 2014, 2022 Stephane Carrez
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
separate (MAT.Targets.Gtkmat)

--  Load the glade XML definition.
procedure Load_UI (Target : in out Target_Type) is
   use type Glib.Guint;

   Result : Glib.Guint;
   Error  : aliased Glib.Error.GError;
begin
   Result := Target.Builder.Add_From_File ("mat.glade", Error'Access);
   if Result /= 1 then
      Log.Error ("Cannot load the 'gakt.glade' configuration file");
      raise Initialize_Error;
   end if;
end Load_UI;
