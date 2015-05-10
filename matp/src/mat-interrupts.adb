-----------------------------------------------------------------------
--  mat-interrupts - SIGINT management to stop long running commands
--  Copyright (C) 2015 Stephane Carrez
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
with Ada.Interrupts;
with Ada.Interrupts.Names;

with Util.Log.Loggers;
package body MAT.Interrupts is

   pragma Interrupt_State (Name  => Ada.Interrupts.Names.SIGINT,
                           State => USER);

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Interrupts");

   protected Interrupts is
      procedure Interrupt;
      pragma Interrupt_Handler (Interrupt);

      function Is_Interrupted return Boolean;

      procedure Clear;

   private
      Interrupted : Boolean;
   end Interrupts;

   protected body Interrupts is

      function Is_Interrupted return Boolean is
      begin
         return Interrupted;
      end Is_Interrupted;

      procedure Clear is
      begin
         Interrupted := False;
      end Clear;

      procedure Interrupt is
      begin
         Interrupted := True;
         Log.Info ("SIGINT signal received");
      end Interrupt;

   end Interrupts;

   --  ------------------------------
   --  Install the SIGINT handler.
   --  ------------------------------
   procedure Install is
   begin
      Ada.Interrupts.Attach_Handler (Interrupts.Interrupt'Access,
                                     Ada.Interrupts.Names.SIGINT);
      Log.Info ("Interrupt handler for SIGINT is installed");
   end Install;

   --  ------------------------------
   --  Reset the interrupted flag.
   --  ------------------------------
   procedure Clear is
   begin
      Interrupts.Clear;
   end Clear;

   --  ------------------------------
   --  Check if we have been interrupted.
   --  ------------------------------
   function Is_Interrupted return Boolean is
   begin
      return Interrupts.Is_Interrupted;
   end Is_Interrupted;

end MAT.Interrupts;
