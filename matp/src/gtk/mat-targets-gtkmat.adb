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
with Glib.Object;

with Gtk.Main;
with Gtk.Label;
with Gtk.Frame;
with Gtk.Scrolled_Window;
with Gtk.Viewport;

with MAT.Callbacks;
with MAT.Consoles.Text;
with MAT.Consoles.Gtkmat;
package body MAT.Targets.Gtkmat is

   --  ------------------------------
   --  Initialize the target instance.
   --  ------------------------------
   overriding
   procedure Initialize (Target : in out Target_Type) is
   begin
      Target.Options.Interactive := False;
      Target.Options.Graphical   := True;
      Target.Console := new MAT.Consoles.Text.Console_Type;
   end Initialize;

   --  ------------------------------
   --  Release the storage.
   --  ------------------------------
   overriding
   procedure Finalize (Target : in out Target_Type) is
      use type Gtk.Widget.Gtk_Widget;
   begin
      if Target.Main /= null then
         Target.Main.Destroy;
         Target.About.Destroy;
         Target.Chooser.Destroy;
      end if;
   end Finalize;

   --  Load the glade XML definition.
   procedure Load_UI (Target : in out Target_Type) is separate;

   --  ------------------------------
   --  Initialize the widgets and create the Gtk gui.
   --  ------------------------------
   procedure Initialize_Widget (Target : in out Target_Type;
                                Widget : out Gtk.Widget.Gtk_Widget) is
      Error    : aliased Glib.Error.GError;
      Result   : Glib.Guint;
      Timeline : Gtk.Widget.Gtk_Widget;
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Viewport : Gtk.Viewport.Gtk_Viewport;
   begin
      if Target.Options.Graphical then
         Gtk.Main.Init;
         Gtkada.Builder.Gtk_New (Target.Builder);
         --           Result := Target.Builder.Add_From_File ("mat.glade", Error'Access);
         Load_UI (Target);
         MAT.Callbacks.Initialize (Target'Unchecked_Access, Target.Builder);
         Target.Builder.Do_Connect;
         Widget := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("main"));
         Target.Main := Widget;
         Target.About := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("about"));
         Target.Chooser := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("filechooser"));

         Target.Gtk_Console := new MAT.Consoles.Gtkmat.Console_Type;
         Target.Gtk_Console.Initialize
           (Gtk.Frame.Gtk_Frame (Target.Builder.Get_Object ("consoleFrame")));
         Target.Console := Target.Gtk_Console.all'Access;
         MAT.Events.Gtkmat.Create (Target.Events);
         Timeline := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("scrolledTimeline"));
         Scrolled := Gtk.Scrolled_Window.Gtk_Scrolled_Window (Timeline);
         Timeline := Gtk.Widget.Gtk_Widget (Target.Builder.Get_Object ("viewport1"));
         Viewport := Gtk.Viewport.Gtk_Viewport (Timeline);
         Viewport.Add (Target.Events.Drawing);
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
   --  Set the UI label with the given value.
   --  ------------------------------
   procedure Set_Label (Target : in Target_Type;
                        Name   : in String;
                        Value  : in String) is
      use type Glib.Object.GObject;

      Object : constant Glib.Object.GObject := Target.Builder.Get_Object (Name);
      Label  : Gtk.Label.Gtk_Label;
   begin
      if Object /= null then
         Label := Gtk.Label.Gtk_Label (Object);
         Label.Set_Label (Value);
      end if;
   end Set_Label;

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
      Target.Set_Label ("process_info", "Pid:" & MAT.Types.Target_Process_Ref'Image (Pid));
   end Create_Process;

   --  ------------------------------
   --  Refresh the information about the current process.
   --  ------------------------------
   procedure Refresh_Process (Target  : in out Target_Type) is
      use type MAT.Events.Targets.Target_Events_Access;

      Counter : Integer;
      Stats   : MAT.Memory.Targets.Memory_Stat;
   begin
      if Target.Current = null or else Target.Current.Events = null then
         return;
      end if;
      Counter := Target.Current.Events.Get_Event_Counter;
      if Counter = Target.Previous_Event_Counter then
         return;
      end if;
      Target.Previous_Event_Counter := Counter;
      Target.Set_Label ("event_info", "Events:" & Integer'Image (Counter));
      Target.Current.Memory.Stat_Information (Stats);
      Target.Set_Label ("thread_info", "Threads:"
                        & Natural'Image (Stats.Thread_Count));
      Target.Set_Label ("mem_used_info", "Used:"
                        & MAT.Types.Target_Size'Image (Stats.Total_Alloc));
      Target.Set_Label ("mem_free_info", "Free:"
                        & MAT.Types.Target_Size'Image (Stats.Total_Free));
      Target.Refresh_Events;
   end Refresh_Process;

   procedure Refresh_Events (Target : in out Target_Type) is
   begin
      --  Target.Events.List.Clear;
      --  MAT.Events.Targets.Get_Events (Target.Current.Events.all, 0, 0, Target.Events.List);
      null;
   end Refresh_Events;

end MAT.Targets.Gtkmat;
