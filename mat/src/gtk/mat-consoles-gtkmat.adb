-----------------------------------------------------------------------
--  mat-consoles-text - Text console interface
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
with Ada.Text_IO;

with Gtk.Enums;
with Gtk.Tree_View_Column;
with Gtk.Scrolled_Window;

with Util.Log.Loggers;
package body MAT.Consoles.Gtkmat is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Consoles.Gtkmat");

   --  ------------------------------
   --  Initialize the console to display the result in the Gtk frame.
   --  ------------------------------
   procedure Initialize (Console : in out Console_Type;
                         Frame   : in Gtk.Frame.Gtk_Frame) is
   begin
      Console.Frame := Frame;
   end Initialize;

   --  Report a notice message.
   overriding
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in String) is
   begin
      null;
   end Notice;

   --  Report an error message.
   overriding
   procedure Error (Console : in out Console_Type;
                    Message : in String) is
   begin
      null;
   end Error;

   --  Print the field value for the given field.
   overriding
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String) is
   begin
      Log.Debug ("Field {0} - {1}", Field_Type'Image (Field), Value);
      Gtk.List_Store.Set (Console.List, Console.Current_Row, Console.Indexes (Field), Value);
   end Print_Field;

   --  Print the title for the given field.
   overriding
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is
      Pos : constant Positive := Console.Field_Count;
   begin
      Console.Indexes (Field) := Glib.Gint (Pos - 1);
      Console.Columns (Pos).Field := Field;
      Console.Columns (Pos).Title := Ada.Strings.Unbounded.To_Unbounded_String (Title);
   end Print_Title;

   --  Start a new title in a report.
   overriding
   procedure Start_Title (Console : in out Console_Type) is
   begin
      Gtk.Cell_Renderer_Text.Gtk_New (Console.Col_Text);
      Console.Indexes := (others => 0);
   end Start_Title;

   --  Finish a new title in a report.
   procedure End_Title (Console : in out Console_Type) is
      use type Glib.Guint;
      use type Glib.Gint;
      use Gtk.List_Store;

      Types : Glib.GType_Array (0 .. Glib.Guint (Console.Field_Count) - 1)
        := (others => Glib.GType_String);
      Col      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num : Glib.Gint;
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Gtk.List_Store.Gtk_New (Console.List, Types);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy (Gtk.Enums.Policy_Always, Gtk.Enums.Policy_Always);

      Gtk.Tree_View.Gtk_New (Console.Tree);
      for I in 1 .. Console.Field_Count loop
         Gtk.Tree_View_Column.Gtk_New (Col);
         Num := Console.Tree.Append_Column (Col);
         Col.Set_Sort_Column_Id (Glib.Gint (I) - 1);
         Col.Set_Title (Ada.Strings.Unbounded.To_String (Console.Columns (I).Title));
         Col.Pack_Start (Console.Col_Text, True);
         Col.Set_Sizing (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
         Col.Add_Attribute (Console.Col_Text, "text", Glib.Gint (I) - 1);
      end loop;
      Scrolled.Add (Console.Tree);
      Scrolled.Show_All;

      Console.Tree.Set_Model (+Console.List);
      Console.Frame.Add (Scrolled);
      Console.Frame.Show_All;
   end End_Title;

   --  Start a new row in a report.
   overriding
   procedure Start_Row (Console : in out Console_Type) is
   begin
      Console.List.Append (Console.Current_Row);
   end Start_Row;

   --  Finish a new row in a report.
   overriding
   procedure End_Row (Console : in out Console_Type) is
   begin
      null;
   end End_Row;

end MAT.Consoles.Gtkmat;
