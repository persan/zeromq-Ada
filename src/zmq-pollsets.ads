-------------------------------------------------------------------------------
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

with Interfaces.C;

with ZMQ.Sockets;
with ZMQ.Low_Level;

--  Implements 0MQ socket input/output multiplexing.
--
--  This package provides a high-level Ada interface to the 0MQ poll
--  mechanism, allowing applications to monitor multiple 0MQ sockets for I/O
--  readiness.
--  Each socket and its associated events of interest must be registered in a
--  Poll_Set before calling the Poll procedure.
--
--  Overview:
--    1. Create a Poll_Set with a maximum size (number of sockets to monitor)
--    2. Create Poll_Item instances for each socket, specifying which events to
--       watch
--    3. Append each Poll_Item to the Poll_Set
--    4. Call Poll to wait for events, with an optional timeout
--    5. Retrieve signaled events using Signaled_Events
--    6. Process ready sockets and repeat
--
--  Example usage:
--    declare
--       Set : ZMQ.Pollsets.Poll_Set (10);
--       Item1 : ZMQ.Pollsets.Poll_Item := (My_Socket_Access, Poll_In);
--       Item2 : ZMQ.Pollsets.Poll_Item := (Other_Socket_Access, Poll_Out);
--       Count : Natural;
--    begin
--       Set.Append (Item1);
--       Set.Append (Item2);
--       Set.Poll (Timeout => 1000, Signaled_Events => Count);
--       --  Process ready sockets from Signaled_Events
--       if Count > 0 then
--          declare
--            Signaled : constant Poll_Items := Set.Signaled_Events;
--          begin
--             --  Do something with signaled events.
--             null;
--          end;
--       end if;
--    end;
--
--  Note: Compared to the low-level zmq_poll function, this package does not
--  support polling of standard sockets based on file descriptors. Only 0MQ
--  sockets are supported, and therefore only 0MQ-specific events are
--  available.
--
--  Thread Safety: Poll_Set operations are NOT thread-safe. Concurrent access
--  to the same Poll_Set from multiple tasks requires external synchronization.
package ZMQ.Pollsets is
   pragma Elaborate_Body;

   type Poll_Flags is mod 2 ** 16;
   for Poll_Flags'Size use 16;
   --  Type representing the set of events that can be polled for on a socket.
   --  This is a bitmask type where individual flags can be combined using
   --  the "or" operator.

   Poll_In  : constant Poll_Flags := ZMQ.Low_Level.Defs.ZMQ_POLLIN;
   --  At least one message may be received from the socket without blocking.
   Poll_Out : constant Poll_Flags := ZMQ.Low_Level.Defs.ZMQ_POLLOUT;
   --  At least one message may be sent to the socket without blocking.

   type Poll_Item is record
      Socket  : ZMQ.Sockets.Any_Socket := null;
      Events  : Poll_Flags := 0;
   end record with
     Dynamic_Predicate => Poll_Item.Events in 0 | Poll_In | Poll_Out |
       (Poll_In or Poll_Out);

   --  Represents a single socket and the events to monitor on it.
   --
   --  Fields:
   --    Socket - The 0MQ socket to monitor. Must be an initialized socket
   --             of any type (PAIR, PUB, SUB, REQ, REP, DEALER, ROUTER, ...).
   --             A null socket is allowed but will never signal any events.
   --    Events - Bitmask of Poll_Flags specifying which events to monitor.
   --             Use Poll_In to check for readability, Poll_Out to check for
   --             writability, or both to check for either condition.

   type Poll_Items is array (Positive range <>) of Poll_Item;
   --  Array type for holding multiple Poll_Item instances. Useful for
   --  iterating over the results returned by Signaled_Events.

   type Poll_Set (Max_Size : Positive) is tagged limited private;
   --  Represents a collection of sockets to poll for I/O readiness.
   --
   --  Each instance has a fixed maximum capacity (Max_Size) specified
   --  at declaration time.
   --
   --  A Poll_Set maintains the set of sockets to monitor and the results of
   --  the most recent Poll operation. After calling Poll, use Signaled_Events
   --  to retrieve which sockets have events ready.
   --
   --  Max_Size is the maximum number of Poll_Item instances this set can hold.

   function Contains
     (This : Poll_Set; Item : Poll_Item) return Boolean;
   --  Checks if a Poll_Item is currently registered in the Poll_Set.

   procedure Append
     (This : in out Poll_Set; Item : Poll_Item) with
     Pre => not Contains (This, Item),
     Post => Contains (This, Item) and then
     This.Signaled_Events'Length = 0;
   --  Adds a Poll_Item to the Poll_Set for monitoring.
   --
   --  Appending an item clears the previous poll state.

   procedure Remove
     (This : in out Poll_Set; Item : Poll_Item) with
     Post => not Contains (This, Item) and then
     This.Signaled_Events'Length = 0;
   --  Removes a Poll_Item from the Poll_Set.
   --
   --  Removing an item clears the previous poll state.

   procedure Poll (This            : in out Poll_Set;
                   Timeout         : Integer;
                   Signaled_Events : out Natural) with
     Pre => Timeout >= -1;
   --  Performs the poll operation, waiting for events on registered sockets.
   --
   --  This procedure blocks until at least one socket has an event ready,
   --  or the timeout expires. After Poll returns, call Signaled_Events to
   --  retrieve the list of sockets with ready events.
   --
   --  Upon completion, Signaled_Events will contain the number of events that
   --  are available, or 0 if there are none.
   --
   --  Example:
   --    declare
   --       Count : Natural;
   --    begin
   --       My_Set.Poll (Timeout => 5000, Signaled_Events => Count);
   --       if Count > 0 then
   --          --  At least one socket is ready
   --       end if;
   --    end;

   function Signaled_Events (This : Poll_Set) return Poll_Items;
   --  Retrieves the list of Poll_Items that have signaled events after a Poll.
   --
   --  This function returns the array of Poll_Items for which events occurred
   --  during the most recent Poll operation. Each returned item's Events field
   --  indicates which specific events were signaled (Poll_In, Poll_Out, or
   --  both).

private
   type C_Poll_Set is array (Positive range <>)
     of aliased ZMQ.Low_Level.zmq_pollitem_t with
       Convention => C;

   use type Interfaces.C.int;

   type Poll_Set (Max_Size : Positive) is tagged limited record
   --  Local_Data and Items arrays are maintained in lockstep, with each
   --  addition and removal performed in both.
      Local_Data         : aliased C_Poll_Set (1 .. Max_Size) :=
                             [others => (socket  => <>,
                                         fd      => -1,
                                         events  => 0,
                                         revents => 0)];
      Items              : Poll_Items (1 .. Max_Size) :=
                             [others => <>];
      Signaled_Events    : Natural := 0;
      Cursor             : Positive := 1;
   end record;

end ZMQ.Pollsets;
