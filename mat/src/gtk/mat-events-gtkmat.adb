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
with Glib;
with Gtk.Handlers;
with Gtk.Widget;

with Util.Log.Loggers;
package body MAT.Events.Gtkmat is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Events.Gtkmat");

   package Event_Cb is
     new Gtk.Handlers.Return_Callback (Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
                                            Boolean);

   Target : Event_Drawing_Type_Access;

   function Redraw (Area : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
                    Cr   : in Cairo.Cairo_Context) return Boolean is
   begin
      Draw (Target.all, Cr);
      return False;
   end Redraw;

   procedure Create (Drawing : in out Event_Drawing_Type) is
   begin
      Target := Drawing'Unrestricted_Access;
      Gtk.Drawing_Area.Gtk_New (Drawing.Drawing);
      Event_Cb.Connect (Drawing.Drawing, Gtk.Widget.Signal_Draw,
                        Event_Cb.To_Marshaller (Redraw'Access)); --  , Drawing'Unrestricted_Access);
   end Create;

   procedure Draw (Onto    : in Event_Drawing_Type;
                   Cr : in Cairo.Cairo_Context) is
      use type Glib.Gdouble;
      use type MAT.Types.Target_Tick_Ref;
      use type MAT.Types.Target_Thread_Ref;

      type Thread_Ref_Array is array (1 .. 10) of MAT.Types.Target_Thread_Ref;

      Iter : MAT.Events.Targets.Target_Event_Cursor;

      Threads : Thread_Ref_Array := (others => 0);
      Start   : MAT.Types.Target_Tick_Ref;
      Finish  : MAT.Types.Target_Tick_Ref;
      Time_F  : Glib.Gdouble;
      Thread_F : Glib.Gdouble;

      function Get_Thread_Index (Thread : in MAT.Types.Target_Thread_Ref) return Natural is
      begin
         for I in Thread_Ref_Array'Range loop
            if Threads (I) = Thread then
               return I;
            end if;
            if Threads (I) = 0 then
               Threads (I) := Thread;
               return I;
            end if;
         end loop;
         return Threads'Last;
      end Get_Thread_Index;

      function Get_Time_Pos (Time : in MAT.Types.Target_Tick_Ref) return Glib.Gdouble is
         Dt : constant MAT.Types.Target_Tick_Ref := Time - Start;
      begin
         return Time_F * Glib.Gdouble (Dt);
      end Get_Time_Pos;

      function Get_Thread_Pos (Thread : in  MAT.Types.Target_Thread_Ref) return Glib.Gdouble is
         Pos : Natural := Get_Thread_Index (Thread);
      begin
         return Thread_F * Glib.Gdouble (Pos);
      end Get_Thread_Pos;

      Width  : Glib.Gint;
      Height : Glib.Gint;
   begin
      Log.Info ("Redraw events window");
      Cairo.Translate (Cr, 10.0, 10.0);
--        Cairo.Scale (Cr, 10.0, 10.0);

      if Onto.List.Is_Empty then
         return;
      end if;

      Width  := Onto.Drawing.Get_Allocated_Width;
      Height := Onto.Drawing.Get_Allocated_Height;
      Start  := Onto.List.First_Element.Time;
      Finish := Onto.List.Last_Element.Time;
      if Start = Finish then
         Time_F := 10.0;
      else
         Time_F := Glib.Gdouble (Width) / Glib.Gdouble (Finish - Start);
      end if;
      Thread_F := Glib.Gdouble (Height) / 10.0;
      Iter := Onto.List.First;
      while MAT.Events.Targets.Target_Event_Vectors.Has_Element (Iter) loop
         declare
            Event : MAT.Events.Targets.Target_Event
              := MAT.Events.Targets.Target_Event_Vectors.Element (Iter);
            Y : Glib.Gdouble := Get_Thread_Pos (Event.Thread);
            X : Glib.Gdouble := Get_Time_Pos (Event.Time);
         begin
            if Event.Event = 4 then
               Cairo.Set_Source_Rgb (Cr, Red => 0.0, Green => 0.0, Blue => 1.0);
            else
               Cairo.Set_Source_Rgb (Cr, Red => 1.0, Green => 0.0, Blue => 0.0);
            end if;
            Cairo.Move_To (Cr, X, Y);
            Cairo.Line_To (Cr, X, Y + 10.0);
            Cairo.Stroke (Cr);
         end;
         MAT.Events.Targets.Target_Event_Vectors.Next (Iter);
      end loop;
--        Cairo.Set_Source_Rgb (Cr, Red => 0.0, Green => 0.0, Blue => 0.0);
--        for I in 1 .. 10 loop
--           Cairo.Move_To (Cr, 10.0 * Glib.Gdouble (I), 0.0);
--           Cairo.Line_To (Cr, 10.0 * Glib.Gdouble (I), 2.0);
--           Cairo.Stroke (Cr);
--        end loop;
   end Draw;

end MAT.Events.Gtkmat;
