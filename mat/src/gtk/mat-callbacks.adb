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

with Glib.Main;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Label;

with Util.Log.Loggers;

with MAT.Commands;
package body MAT.Callbacks is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Callbacks");

   type Info is record
      Builder : access Gtkada.Builder.Gtkada_Builder_Record'Class;
   end record;

   package Timer_Callback is
     new Glib.Main.Generic_Sources (MAT.Targets.Gtkmat.Target_Type_Access);

   Timer : Glib.Main.G_Source_Id;
   MemTotal : Natural := 1;

   Target : MAT.Targets.Gtkmat.Target_Type_Access;

   function Refresh_Timeout (Target : in MAT.Targets.Gtkmat.Target_Type_Access) return Boolean is
   begin
      Target.Refresh_Process;
      return True;
   end Refresh_Timeout;

   --  ------------------------------
   --  Initialize and register the callbacks.
   --  ------------------------------
   procedure Initialize (Target  : in MAT.Targets.Gtkmat.Target_Type_Access;
                         Builder : in Gtkada.Builder.Gtkada_Builder) is
   begin
      MAT.Callbacks.Target := Target;
      Builder.Register_Handler (Handler_Name => "quit",
                                Handler      => MAT.Callbacks.On_Menu_Quit'Access);
      Builder.Register_Handler (Handler_Name => "about",
                                Handler      => MAT.Callbacks.On_Menu_About'Access);
      Builder.Register_Handler (Handler_Name => "close-about",
                                Handler      => MAT.Callbacks.On_Close_About'Access);
      Builder.Register_Handler (Handler_Name => "cmd-sizes",
                                Handler      => MAT.Callbacks.On_Btn_Sizes'Access);
      Timer := Timer_Callback.Timeout_Add (1000, Refresh_Timeout'Access, Target);
   end Initialize;

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

   --  ------------------------------
   --  Callback executed when the "cmd-sizes" action is executed from the "Sizes" action.
   --  ------------------------------
   procedure On_Btn_Sizes (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Object);
   begin
      MAT.Commands.Sizes_Command (Target.all, "");
   end On_Btn_Sizes;

end MAT.Callbacks;
