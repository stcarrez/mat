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
package MAT.Targets.Gtkmat is

   type Target_Type is new MAT.Targets.Target_Type with private;
   type Target_Type_Access is access all Target_Type'Class;

   --  Initialize the target instance.
   overriding
   procedure Initialize (Target : in out Target_Type);

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

private

   task type Gtk_Loop is
      entry Start (Target : in Target_Type_Access);
   end Gtk_Loop;

   type Target_Type is new MAT.Targets.Target_Type with record
      Builder  : Gtkada.Builder.Gtkada_Builder;
      Gui_Task : Gtk_Loop;
   end record;

end MAT.Targets.Gtkmat;
