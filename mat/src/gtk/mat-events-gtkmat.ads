-----------------------------------------------------------------------
--  gprofiler-events - Profiler Events Description
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

with Cairo;
with Gtk.Drawing_Area;

with MAT.Events.Targets;
package MAT.Events.Gtkmat is

   type Event_Drawing_Type is record
      Drawing : Gtk.Drawing_Area.Gtk_Drawing_Area;
      List    : MAT.Events.Targets.Target_Event_Vector;
   end record;
   type Event_Drawing_Type_Access is access Event_Drawing_Type;

   procedure Create (Drawing : in out Event_Drawing_Type);

   procedure Draw (Onto : in Event_Drawing_Type;
                   Cr   : in Cairo.Cairo_Context);

end MAT.Events.Gtkmat;
