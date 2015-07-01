------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: ISC License (see COPYING file)                                  --
--                                                                          --
--                        Copyright © 2015 darkestkhan                      --
------------------------------------------------------------------------------
-- Permission to use, copy, modify, and/or distribute this software for any --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- The software is provided "as is" and the author disclaims all warranties --
-- with regard to this software including all implied warranties of         --
-- merchantability and fitness. In no event shall the author be liable for  --
-- any special, direct, indirect, or consequential damages or any damages   --
-- whatsoever resulting from loss of use, data or profits, whether in an    --
-- action of contract, negligence or other tortious action, arising out of  --
-- or in connection with the use or performance of this software.           --
------------------------------------------------------------------------------
with Ada.Containers.Indefinite_Vectors;

  ------------------------------------------------------------------------------
  -- Small and simple callback based library for processing program arguments --
  ------------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- First register all callbacks you are interested in, then call
  -- Process_Arguments.
  --
  -- NOTE: "=" sign can't be part of argument name for registered callback.
  --
  -- Only one callback can be registered per argument, otherwise
  -- Constraint_Error is propagated.
  ---------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- Leading single hyphen and double hyphen are stripped (if present) when
  -- processing arguments that are placed before first "--" argument,
  -- so ie. "--help", "-help" and "help" are functionally
  -- equivalent, treated as if "help" was actually passed in all 3 cases.
  -- (you should register callback just for "help", if you register it for
  -- "--help" then actually passed argument would have to be "----help" in order
  -- for it to trigger said callback)
  --
  -- In addition if you registered callback as case insensitive, then
  -- (using above example) "Help", "HELP", "help" and "HeLp" all would result in
  -- call to said callback.
  --
  -- If Argument_Type is Variable then callback will receive part of argument
  -- that is after "=" sign. (ie. "OS=Linux" argument would result in callback
  -- receiving only "Linux" as its argument).
  --
  -- For Variable, case insensitive callbacks simple rule applies:
  -- only variable name is case insensitive, with actual value (when passed to
  -- program) being unchanged.
  --
  -- All arguments with no associated callback are added to Unknown_Arguments
  --  vector as long as they appear before first "--" argument.
  --
  -- All arguments after first "--" (standalone double hyphen) are added to
  --  Input_Argument vector (this includes another "--"), w/o any kind of
  --  hyphen stripping being performed on them.
  --
  -- NOTE: No care is taken to ensure that all Input_Arguments
  --  (or Unknown_Arguments) are unique.
  ---------------------------------------------------------------------------

package CBAP is

  ---------------------------------------------------------------------------
  -- Yes, I do realize this is actually vector...
  package Argument_Lists is new
    Ada.Containers.Indefinite_Vectors (Positive, String);

  -- List of all arguments for which callbacks where not registered.
  Unknown_Arguments : Argument_Lists.Vector := Argument_Lists.Empty_Vector;
  -- List of all arguments after first "--" argument.
  Input_Arguments   : Argument_Lists.Vector := Argument_Lists.Empty_Vector;

  -- Argument_Types decides if argument is just a simple value, or a variable
  -- with value assigned to it (difference between "--val" and "--var=val")
  type Argument_Types is (Value, Variable);

  -- Argument is useful mostly for Variable type of arguments.
  type Callbacks is not null access procedure (Argument: in String);

  ---------------------------------------------------------------------------
  -- Register callbacks for processing arguments.
  -- @Callback      : Action to be performed in case appropriate argument is
  --    detected.
  -- @Called_On     : Argument for which callback is to be performed.
  -- @Argument_Type : {Value, Variable}. Value is simple argument that needs no
  --    additional parsing. Variable is argument of "Arg_Name=Some_Val" form.
  -- @Case_Sensitive: Whether or not case is significant. When False all forms
  --    of argument are treated as if written in lower case.
  procedure Register
    ( Callback      : in Callbacks;
      Called_On     : in String;
      Argument_Type : in Argument_Types := Value;
      Case_Sensitive: in Boolean        := True
    );

  -- Raised if Called_On contains "=".
  Incorrect_Called_On: exception;

  ---------------------------------------------------------------------------
  -- Parse arguments supplied to program, calling callbacks when argument with
  -- associated callback is detected.
  -- NOTE: Process_Arguments will call callbacks however many times argument
  --  with associated callback is called. So if you have callback for "help",
  --  then ./program help help help
  --  will result in 3 calls to said callback.
  procedure Process_Arguments;

  ---------------------------------------------------------------------------

end CBAP;
