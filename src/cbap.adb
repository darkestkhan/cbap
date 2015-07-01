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
with Ada.Characters.Handling;
with Ada.Containers.Hashed_Sets;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
package body CBAP is

  ---------------------------------------------------------------------------

  function To_Lower (Item: in String) return String
    renames Ada.Characters.Handling.To_Lower;

  ---------------------------------------------------------------------------

  type Callback_Data is
  record
    Hash    : Ada.Containers.Hash_Type;
    Callback: Callbacks;
    Arg_Type: Argument_Types  := Value;
    Case_Sensitive: Boolean   := True;
  end record;

  function Hash (This: in Callback_Data) return Ada.Containers.Hash_Type
  is
  begin
    return This.Hash;
  end Hash;

  overriding function "=" (Left, Right: in Callback_Data) return Boolean
  is
    use type Ada.Containers.Hash_Type;
  begin
    return Left.Hash = Right.Hash;
  end "=";

  procedure Null_Callback (Item: in String) is null;

  Empty_Callback_Data: constant Callback_Data :=
    Callback_Data'(0, Null_Callback'Access, Value, True);

  package Callback_Sets is new
    Ada.Containers.Hashed_Sets (Callback_Data, Hash, "=");

  ---------------------------------------------------------------------------

  Callback_Set: Callback_Sets.Set := Callback_Sets.Empty_Set;

  ---------------------------------------------------------------------------

  procedure Register
    ( Callback      : in Callbacks;
      Called_On     : in String;
      Argument_Type : in Argument_Types := Value;
      Case_Sensitive: in Boolean        := True
    )
  is
    Var: Callback_Data :=
      Callback_Data'(0, Callback, Argument_Type, Case_Sensitive);
  begin
    if Ada.Strings.Fixed.Index (Called_On, "=", Called_On'First) /= 0 then
      raise Incorrect_Called_On with Called_On;
    end if;

    if Case_Sensitive then
      Var.Hash := Ada.Strings.Hash (Called_On);
    else
      Var.Hash := Ada.Strings.Hash (To_Lower (Called_On));
    end if;

    Callback_Set.Insert (Var);
  exception
    when Constraint_Error => raise Constraint_Error with
      "Following argument already has callback registered: " & Called_On;
  end Register;

  ---------------------------------------------------------------------------

  procedure Process_Arguments
  is

    ---------------------------------------------------------------------------

    package CLI renames Ada.Command_Line;

    ---------------------------------------------------------------------------

    procedure Add_Inputs (From: in Natural)
    is
    begin
      if not (From < CLI.Argument_Count) then
        return;
      end if;

      for K in From + 1 .. CLI.Argument_Count loop
        Input_Arguments.Append (CLI.Argument (K));
      end loop;
    end Add_Inputs;

    ---------------------------------------------------------------------------

    function Extract_Name   (Item: in String) return String
    is
    begin
      return Item
        (Item'First .. Ada.Strings.Fixed.Index (Item, "=", Item'First) - 1);
    end Extract_Name;

    ---------------------------------------------------------------------------

    function Extract_Value  (Item: in String) return String
    is
    begin
      return Item
        (Ada.Strings.Fixed.Index (Item, "=", Item'First) + 1 .. Item'Last);
    end Extract_Value;

    ---------------------------------------------------------------------------

    procedure Check_Callback (Item: in String)
    is
      Dummy_Callback  : Callback_Data := Empty_Callback_Data;
      Actual_Callback : Callback_Data := Empty_Callback_Data;
    begin
      -- Check for case insensitive value callback
      Dummy_Callback.Hash := Ada.Strings.Hash (To_Lower (Item));
      if Callback_Set.Contains (Dummy_Callback) then
        Actual_Callback :=
          Callback_Sets.Element (Callback_Set.Find (Dummy_Callback));

        if not (Actual_Callback.Case_Sensitive) and
          Actual_Callback.Arg_Type = Value
        then
          Actual_Callback.Callback (Item);
          return;
        end if;
      end if;

      -- Check for case sensitive value callback
      Dummy_Callback.Hash := Ada.Strings.Hash (Item);
      if Callback_Set.Contains (Dummy_Callback) then
        Actual_Callback :=
          Callback_Sets.Element (Callback_Set.Find (Dummy_Callback));

        if Actual_Callback.Arg_Type = Value and
          Actual_Callback.Case_Sensitive
        then
          Actual_Callback.Callback (Item);
          return;
        end if;
      end if;

      -- Check for case insensitive variable callback
      Dummy_Callback.Hash := Ada.Strings.Hash (To_Lower (Extract_Name (Item)));
      if Callback_Set.Contains (Dummy_Callback) then
        Actual_Callback :=
          Callback_Sets.Element (Callback_Set.Find (Dummy_Callback));

        if not (Actual_Callback.Case_Sensitive) and
          Actual_Callback.Arg_Type = Variable
        then
          Actual_Callback.Callback (Extract_Value (Item));
          return;
        end if;
      end if;

      -- Check for case sensitive variable callback
      Dummy_Callback.Hash := Ada.Strings.Hash (Extract_Name (Item));
      if Callback_Set.Contains (Dummy_Callback) then
        Actual_Callback :=
          Callback_Sets.Element (Callback_Set.Find (Dummy_Callback));

        if Actual_Callback.Arg_Type = Variable and
          Actual_Callback.Case_Sensitive
        then
          Actual_Callback.Callback (Extract_Value (Item));
          return;
        end if;
      end if;

      -- No callback associated with argument has been found.
      Unknown_Arguments.Append (Item);
    end Check_Callback;

    ---------------------------------------------------------------------------

  begin
    if CLI.Argument_Count = 0 then
      return;
    end if;

    for K in 1 .. CLI.Argument_Count loop
      declare
        Arg: constant String := CLI.Argument (K);
      begin
        if Arg = "--" then
          Add_Inputs (K);
          return;
        end if;

        -- Strip leading hyphens
        if Arg'Length > 2 and then Arg (Arg'First .. Arg'First + 1) = "--" then
          Check_Callback (Arg (Arg'First + 2 .. Arg'Last));
        elsif Arg'Length > 1 and then Arg (1 .. 1) = "-" then
          Check_Callback (Arg (Arg'First + 1 .. Arg'Last));
        else
          Check_Callback (Arg);
        end if;
      end;
    end loop;
  end Process_Arguments;

  ---------------------------------------------------------------------------

end CBAP;
