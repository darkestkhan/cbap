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
procedure CBAP_Variable_Detection is

  ---------------------------------------------------------------------------

  Error_Count: Natural := 0;

  ---------------------------------------------------------------------------

  procedure Case_Insensitive (Variable: in String)
  is
  begin
    if Variable /= "TruE" then
      Ada.Text_IO.Put_Line
        ( Ada.Text_IO.Standard_Error, "Failed at Case_Insensitive" );
      Ada.Text_IO.Put_Line (Variable);
      Error_Count := Error_Count + 1;
    end if;
  end Case_Insensitive;

  ---------------------------------------------------------------------------

  procedure Case_Sensitive (Variable: in String)
  is
  begin
    if Variable /= "true" then
      Ada.Text_IO.Put_Line
        ( Ada.Text_IO.Standard_Error, "Failed at Case_Sensitive" );
      Error_Count := Error_Count + 1;
    end if;
  end Case_Sensitive;

  ---------------------------------------------------------------------------

begin
  CBAP.Register
    ( Case_Insensitive'Unrestricted_Access,
      "insensitive",
      CBAP.Variable,
      Case_Sensitive => False
    );
  CBAP.Register
    ( Case_Sensitive'Unrestricted_Access, "SENSITIVE", CBAP.Variable );
  CBAP.Process_Arguments;

  if Error_Count /= 0 then
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
  else
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
  end if;
end CBAP_Variable_Detection;
