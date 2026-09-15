-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . P O L L S E T S                          --
--                                                                           --
--                                  S p e c                                  --
--                                                                           --
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

with GNAT.OS_Lib;

with ZMQ.Errors;

package body ZMQ.Pollsets is
   use Interfaces.C;
   --  use type ZMQ.Sockets.Any_Socket;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (This : in out Poll_Set) is
   begin
      This.Signaled_Events := 0;
   end Reset_State;

   --------------
   -- Contains --
   --------------

   function Contains
     (This : Poll_Set; Item : Poll_Item) return Boolean
   is
   begin
      for Element of This.Items loop
         if Element = Item then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Poll_Set; Item : Poll_Item) is
   begin
      if This.Cursor > This.Max_Size then
         raise Program_Error with "Array overflow with cursor = " &
           This.Cursor'Image;
      end if;

      This.Local_Data (This.Cursor) := (socket  => Item.Socket.Get_Impl,
                                        fd      => -1,
                                        events  => short (Item.Events),
                                        revents => 0);
      This.Items (This.Cursor) := Item;
      This.Cursor := This.Cursor + 1;

      This.Reset_State;

      pragma Assert (This.Cursor > 1 and then This.Cursor <= This.Max_Size,
                     "Invalid calculated cursor position");
   end Append;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This : in out Poll_Set; Item : Poll_Item) is
      Item_Position : Natural := 0;
   begin
      --  Search for Item in the Items array.
      for I in This.Items'Range loop
         if This.Items (I) = Item then
            Item_Position := I;
            exit;
         end if;

         pragma Loop_Invariant (Item_Position <= This.Max_Size);
      end loop;

      --  Return if not found.
      if Item_Position = 0 then
         return;
      end if;

      --  Update cursor and copy last element to the found item slot.
      This.Cursor := This.Cursor - 1;
      This.Local_Data (Item_Position) := This.Local_Data (This.Cursor);
      This.Items (Item_Position) := This.Items (This.Cursor);

      --  Clear previous last element by initializing it.
      This.Local_Data (This.Cursor) := (socket  => <>,
                                        fd      => -1,
                                        events  => 0,
                                        revents => 0);
      This.Items (This.Cursor) := (others => <>);

      This.Reset_State;

      pragma Assert (This.Cursor >= 1 and then This.Cursor < This.Max_Size,
                     "Invalid calculated cursor position");
   end Remove;

   ----------
   -- Poll --
   ----------

   procedure Poll
     (This            : in out Poll_Set;
      Timeout         : Integer;
      Signaled_Events : out Natural)
   is
      Ret  : int;
   begin
      This.Signaled_Events := 0;

      Ret := Low_Level.zmq_poll
        (items_u   => This.Local_Data (1)'Unrestricted_Access,
         nitems_u  => int (This.Cursor - 1),
         timeout_u => long (Timeout));

      --  Values greater or equal to zero indicate number of poll items with
      --  signaled events. Other values indicate an error condition.
      if Ret < 0 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
      end if;

      Signaled_Events := Natural (Ret);
      This.Signaled_Events := Signaled_Events;
   end Poll;

   ---------------------
   -- Signaled_Events --
   ---------------------

   function Signaled_Events (This : Poll_Set) return Poll_Items is
      R_Index : Positive := 1;
   begin
      return R : Poll_Items (1 .. This.Signaled_Events) do
         for I in This.Local_Data'Range loop
            pragma Loop_Invariant (R_Index <= I);

            if This.Local_Data (I).revents > 0 then
               R (R_Index) := This.Items (I);
               R_Index := R_Index + 1;
            end if;
         end loop;
      end return;
   end Signaled_Events;

end ZMQ.Pollsets;
