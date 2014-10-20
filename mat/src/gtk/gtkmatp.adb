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

with Util.Log.Loggers;

with Readline;

with MAT.Commands;
with MAT.Targets;
with MAT.Consoles.Text;
with MAT.Callbacks;

with Glib.Error;
with Gtk.Main;
with Gtk.Widget;
with Gtkada.Builder;
procedure GtkMatp is

   Builder : Gtkada.Builder.Gtkada_Builder;
   Error   : aliased Glib.Error.GError;
   Result  : Glib.Guint;
   Main    : Gtk.Widget.Gtk_Widget;
begin
   Gtk.Main.Init;
   Util.Log.Loggers.Initialize ("matp.properties");
   Gtkada.Builder.Gtk_New (Builder);
   Result := Builder.Add_From_File ("mat.glade", Error'Access);
   Builder.Register_Handler (Handler_Name => "quit",
                             Handler      => MAT.Callbacks.On_Menu_Quit'Access);
   Builder.Register_Handler (Handler_Name => "about",
                             Handler      => MAT.Callbacks.On_Menu_About'Access);
   Builder.Do_Connect;
   Main := Gtk.Widget.Gtk_Widget (Builder.Get_Object ("main"));
   Main.Show_All;
   Gtk.Main.Main;
end GtkMatp;
