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
with Glib;
with Gtk.Frame;
with Gtk.List_Store;
with Gtk.Tree_Model;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_View;
with Gtk.Scrolled_Window;

package MAT.Consoles.Gtkmat is

   type Console_Type is new MAT.Consoles.Console_Type with private;
   type Console_Type_Access is access all Console_Type'Class;

   --  Initialize the console to display the result in the Gtk frame.
   procedure Initialize (Console : in out Console_Type;
                         Frame   : in Gtk.Frame.Gtk_Frame);

   --  Report a notice message.
   overriding
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in String);

   --  Report an error message.
   overriding
   procedure Error (Console : in out Console_Type;
                    Message : in String);

   --  Print the field value for the given field.
   overriding
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String);

   --  Print the title for the given field.
   overriding
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String);

   --  Start a new title in a report.
   overriding
   procedure Start_Title (Console : in out Console_Type);

   --  Finish a new title in a report.
   procedure End_Title (Console : in out Console_Type);

   --  Start a new row in a report.
   overriding
   procedure Start_Row (Console : in out Console_Type);

   --  Finish a new row in a report.
   overriding
   procedure End_Row (Console : in out Console_Type);

private

   type Column_Type is record
      Field : Field_Type;
      Title : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Column_Array is array (1 .. Field_Type'Pos (Field_Type'Last)) of Column_Type;

   type Field_Index_Array is array (Field_Type) of Glib.Gint;

   type Console_Type is new MAT.Consoles.Console_Type with record
      Frame       : Gtk.Frame.Gtk_Frame;
      Scrolled    : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      List        : Gtk.List_Store.Gtk_List_Store;
      Current_Row : Gtk.Tree_Model.Gtk_Tree_Iter;
      File        : Ada.Text_IO.File_Type;
      Indexes     : Field_Index_Array;
      Columns     : Column_Array;
      Tree        : Gtk.Tree_View.Gtk_Tree_View;
      Col_Text    : Gtk.Cell_Renderer_Text.Gtk_Cell_renderer_Text;
   end record;

end MAT.Consoles.Gtkmat;
