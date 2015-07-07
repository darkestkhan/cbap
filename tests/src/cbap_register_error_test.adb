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
with CBAP;

procedure CBAP_Register_Error_Test is

  ---------------------------------------------------------------------------

  procedure Cause_Error (Argument: in String) is null;

  ---------------------------------------------------------------------------

  Got_Errors: Natural := 0;

  ---------------------------------------------------------------------------

begin
  begin
    CBAP.Register (Cause_Error'Unrestricted_Access, "letsroll=die");
  exception
    when CBAP.Incorrect_Called_On => Got_Errors := Got_Errors + 1;
  end;

  begin
    CBAP.Register (Cause_Error'Unrestricted_Access, "help");
    CBAP.Register (Cause_Error'Unrestricted_Access, "help");
  exception
    when Constraint_Error => Got_Errors := Got_Errors + 1;
  end;

  if Got_Errors /= 2 then
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
  else
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
  end if;
end CBAP_Register_Error_Test;
