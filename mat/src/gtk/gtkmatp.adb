-----------------------------------------------------------------------
--  gtkmatp -- Gtk MAT application
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
with Ada.IO_Exceptions;

with MAT.Commands;
with MAT.Targets;
with MAT.Consoles.Text;
with MAT.Targets.Gtkmat;

with Gtk.Widget;
procedure GtkMatp is
   Main    : Gtk.Widget.Gtk_Widget;
   Target  : MAT.Targets.Gtkmat.Target_Type;
begin
   Target.Initialize_Options;
   Target.Initialize_Widget (Main);
   MAT.Commands.Initialize_Files (Target);
   Target.Start;
   Target.Interactive;
   Target.Stop;
end GtkMatp;
