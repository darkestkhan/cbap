pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see COPYING file)                                                       --
--                                                                          --
--                    Copyright © 2015 darkestkhan                          --
------------------------------------------------------------------------------
--  This Program is Free Software: You can redistribute it and/or modify    --
--  it under the terms of The GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the license, or     --
--                (at Your option) any later version.                       --
--                                                                          --
--      This Program is distributed in the hope that it will be useful,     --
--      but WITHOUT ANY WARRANTY; without even the implied warranty of      --
--      MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the        --
--              GNU General Public License for more details.                --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--   along with this program. If not, see <http://www.gnu.org/licenses/>.   --
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO;

with CBAP;
procedure CBAP_Inputs_Detection is

  ---------------------------------------------------------------------------
  -- Dummy callback.
  procedure Help (Argument : in String) is null;

  ---------------------------------------------------------------------------

  Input_Args : constant array (Positive range <>) of access String :=
    ( new String'("doing"), new String'("----"), new String'("--"),
      new String'("help"),  new String'("done")
    );

  ---------------------------------------------------------------------------

  Error_Count     : Natural := 0;
  Detected_Inputs : Natural := 0;

  procedure Check_Args (Position : in CBAP.Argument_Lists.Cursor)
  is

    ---------------------------------------------------------------------------

    function Is_Correct_Input_Arg (Arg : in String) return Boolean
    is
    begin
      for K in Input_Args'Range loop
        if Input_Args (K).all = Arg then
          return True;
        end if;
      end loop;
      Ada.Text_IO.Put_Line ("Detected wrong argument: " & Arg);
      return False;
    end Is_Correct_Input_Arg;

    ---------------------------------------------------------------------------

  begin
    if not Is_Correct_Input_Arg (CBAP.Argument_Lists.Element (Position)) then
      Error_Count := Error_Count + 1;
    else
      Detected_Inputs := Detected_Inputs + 1;
    end if;
  end Check_Args;

  ---------------------------------------------------------------------------

begin
  CBAP.Register (Help'Unrestricted_Access, "help");
  CBAP.Process_Arguments;

  CBAP.Input_Arguments.Iterate (Check_Args'Unrestricted_Access);

  if Detected_Inputs /= Input_Args'Length then
    Error_Count := Error_Count + 1;
  end if;

  if Error_Count > 0 then
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
  else
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
  end if;
end CBAP_Inputs_Detection;
