-----------------------------------------------------------------------
--  mat-callbacks - Callbacks for Gtk
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

with Gtk.Main;
with Gtk.Widget;
package body MAT.Callbacks is

   --  ------------------------------
   --  Callback executed when the "quit" action is executed from the menu.
   --  ------------------------------
   procedure On_Menu_Quit (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end On_Menu_Quit;

   --  ------------------------------
   --  Callback executed when the "about" action is executed from the menu.
   --  ------------------------------
   procedure On_Menu_About (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class) is
      About : constant Gtk.Widget.Gtk_Widget :=
        Gtk.Widget.Gtk_Widget (Object.Get_Object ("about"));
   begin
      About.Show;
   end On_Menu_About;

   --  ------------------------------
   --  Callback executed when the "close-about" action is executed from the about box.
   --  ------------------------------
   procedure On_Close_About (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class) is
      About : constant Gtk.Widget.Gtk_Widget :=
        Gtk.Widget.Gtk_Widget (Object.Get_Object ("about"));
   begin
      About.Hide;
   end On_Close_About;

end MAT.Callbacks;