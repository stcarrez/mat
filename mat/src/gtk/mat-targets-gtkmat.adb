-----------------------------------------------------------------------
--  mat-targets-gtkmat - Gtk target management
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
with Glib.Error;
with Gtk.Main;

with MAT.Callbacks;
package body MAT.Targets.Gtkmat is

   --  ------------------------------
   --  Initialize the target instance.
   --  ------------------------------
   overriding
   procedure Initialize (Target : in out Target_Type) is
   begin
      Target.Options.Interactive := False;
      Target.Options.Graphical   := True;
   end Initialize;

   --  ------------------------------
   --  Initialize the widgets and create the Gtk gui.
   --  ------------------------------
   procedure Initialize_Widget (Target : in out Target_Type;
                                Widget : out Gtk.Widget.Gtk_Widget) is
      Error   : aliased Glib.Error.GError;
      Result  : Glib.Guint;
   begin
      if Target.Options.Graphical then
         Gtk.Main.Init;
         Gtkada.Builder.Gtk_New (Target.Builder);
         Result := Target.Builder.Add_From_File ("mat.glade", Error'Access);
         MAT.Callbacks.Initialize (Target.Builder);
         Target.Builder.Do_Connect;
         Widget := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("main"));
      else
         Widget := null;
      end if;
   end Initialize_Widget;

   --  ------------------------------
   --  Enter in the interactive loop reading the commands from the standard input
   --  and executing the commands.
   --  ------------------------------
   overriding
   procedure Interactive (Target : in out Target_Type) is
      Main : Gtk.Widget.Gtk_Widget;
   begin
      if Target.Options.Graphical then
         Main := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("main"));
         Main.Show_All;
      end if;
      if Target.Options.Interactive and Target.Options.Graphical then
         Target.Gui_Task.Start (Target'Unchecked_Access);
      end if;
      if Target.Options.Graphical then
         Gtk.Main.Main;
      else
         MAT.Targets.Target_Type (Target).Interactive;
      end if;
   end Interactive;

   task body Gtk_Loop is
      Main : Target_Type_Access;
   begin
      select
         accept Start (Target : in Target_Type_Access) do
            Main := Target;
         end Start;
         MAT.Targets.Target_Type (Main.all).Interactive;
      or
         terminate;
      end select;
   end Gtk_Loop;

   --  ------------------------------
   --  Create a process instance to hold and keep track of memory and other information about
   --  the given process ID.
   --  ------------------------------
   overriding
   procedure Create_Process (Target  : in out Target_Type;
                             Pid     : in MAT.Types.Target_Process_Ref;
                             Path    : in Ada.Strings.Unbounded.Unbounded_String;
                             Process : out Target_Process_Type_Access) is
   begin
      MAT.Targets.Target_Type (Target).Create_Process (Pid, Path, Process);
   end Create_Process;

end MAT.Targets.Gtkmat;
