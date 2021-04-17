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

with Gtk.Widget;
with Gtkada.Builder;

with MAT.Events.Gtkmat;
with MAT.Consoles.Gtkmat;
package MAT.Targets.Gtkmat is

   type Target_Type is new MAT.Targets.Target_Type with private;
   type Target_Type_Access is access all Target_Type'Class;

   --  Initialize the target instance.
   overriding
   procedure Initialize (Target : in out Target_Type);

   --  Release the storage.
   overriding
   procedure Finalize (Target : in out Target_Type);

   --  Initialize the widgets and create the Gtk gui.
   procedure Initialize_Widget (Target : in out Target_Type;
                                Widget : out Gtk.Widget.Gtk_Widget);

   --  Create a process instance to hold and keep track of memory and other information about
   --  the given process ID.
   overriding
   procedure Create_Process (Target  : in out Target_Type;
                             Pid     : in MAT.Types.Target_Process_Ref;
                             Path    : in Ada.Strings.Unbounded.Unbounded_String;
                             Process : out Target_Process_Type_Access);

   --  Enter in the interactive loop reading the commands from the standard input
   --  and executing the commands.
   overriding
   procedure Interactive (Target : in out Target_Type);

   --  Set the UI label with the given value.
   procedure Set_Label (Target : in Target_Type;
                        Name   : in String;
                        Value  : in String);

   --  Refresh the information about the current process.
   procedure Refresh_Process (Target  : in out Target_Type);

private

   --  Load the glade XML definition.
   procedure Load_UI (Target : in out Target_Type);

   task type Gtk_Loop is
      entry Start (Target : in Target_Type_Access);
   end Gtk_Loop;

   type Target_Type is new MAT.Targets.Target_Type with record
      Builder     : Gtkada.Builder.Gtkada_Builder;
      Gui_Task    : Gtk_Loop;
      Previous_Event_Counter : Integer := 0;
      Gtk_Console : MAT.Consoles.Gtkmat.Console_Type_Access;
      Main        : Gtk.Widget.Gtk_Widget;
      About       : Gtk.Widget.Gtk_Widget;
      Chooser     : Gtk.Widget.Gtk_Widget;
      Events      : MAT.Events.Gtkmat.Event_Drawing_Type;
   end record;

   procedure Refresh_Events (Target : in out Target_Type);

end MAT.Targets.Gtkmat;
