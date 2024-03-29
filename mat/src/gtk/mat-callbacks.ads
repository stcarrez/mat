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
with Gtkada.Builder;
with Gtk.Widget;

with MAT.Targets.Gtkmat;
package MAT.Callbacks is

   --  Initialize and register the callbacks.
   procedure Initialize (Target  : in MAT.Targets.Gtkmat.Target_Type_Access;
                         Builder : in Gtkada.Builder.Gtkada_Builder);

   --  Callback executed when the "quit" action is executed from the menu.
   procedure On_Menu_Quit (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class);

   --  Callback executed when the "about" action is executed from the menu.
   procedure On_Menu_About (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class);

   --  Callback executed when the "close-about" action is executed from the about box.
   procedure On_Close_About (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class);

   --  Callback executed when the "cmd-sizes" action is executed from the "Sizes" action.
   procedure On_Btn_Sizes (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class);

   --  Callback executed when the "cmd-threads" action is executed from the "Threads" action.
   procedure On_Btn_Threads (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class);

   --  Close the main window application and terminate.
   function On_Delete_Main (Object : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;

end MAT.Callbacks;
