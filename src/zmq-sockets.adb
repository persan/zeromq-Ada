with ZMQ.Low_Level;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with GNAT.Source_Info;
package body ZMQ.Sockets is
   use Interfaces.C.Strings;
   use Interfaces.c;
   use System;
   map : constant array (Socket_Opt) of int :=
           (HWM          => Low_Level.Defs.ZMQ_HWM,   -- Set high water mark
            LWM          => Low_Level.Defs.ZMQ_LWM,   -- Set low water mark
            SWAP         => Low_Level.Defs.ZMQ_SWAP,
            AFFINITY     => Low_Level.Defs.ZMQ_AFFINITY,
            IDENTITY     => Low_Level.Defs.ZMQ_IDENTITY,
            SUBSCRIBE    => Low_Level.Defs.ZMQ_SUBSCRIBE,
            UNSUBSCRIBE  => Low_Level.Defs.ZMQ_UNSUBSCRIBE,
            RATE         => Low_Level.Defs.ZMQ_RATE,
            RECOVERY_IVL => Low_Level.Defs.ZMQ_RECOVERY_IVL,
            MCAST_LOOP   => Low_Level.Defs.ZMQ_MCAST_LOOP,
            SNDBUF       => Low_Level.Defs.ZMQ_SNDBUF,
            RCVBUF       => Low_Level.Defs.ZMQ_RCVBUF);
   ----------------
   -- Initialize --
   ----------------
   not overriding procedure Initialize
     (This         : in out Socket;
      With_Context : Contexts.Context;
      Kind         : Socket_Type)
   is
   begin
      if With_Context.getImpl = Null_Address then
         raise ZMQ_Error with "Contecxt Not Initialized";
      end if;
      if This.c /= Null_Address then
         raise ZMQ_Error with "Socket Initialized";
      end if;

      This.c := Low_Level.zmq_socket (With_Context.getImpl,
                                      Socket_Type'Pos (Kind));
      if This.c = Null_Address then
         raise ZMQ_Error with "Unable to initialize";
      end if;
   end Initialize;

   ----------
   -- Bind --
   ----------

   not overriding procedure Bind
     (This    : in out Socket;
      Address : String)
   is
      addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      ret  : Int;
   begin
      ret := Low_Level.zmq_bind (this.c, addr);
      Free (addr);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end Bind;

   procedure  setsockopt (This       : in out Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : Natural) is
      ret     : Int;
   begin
      ret := Low_Level.zmq_setsockopt
        (this.c,
         map (Option),
         Value,
         Value_Size);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure setsockopt
     (This         : in out Socket;
      Option       : Socket_Opt;
      Value   : String)
   is
   begin
      This.setsockopt (Option,Value (Value'First)'Address, Value'Length);
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Boolean)
   is
   begin
      This.setsockopt (Option,Value'Address, 1);
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Natural)
   is
   begin
      This.setsockopt (Option,Value'Address, 4);
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------
--     function img ( item : Ada.Streams.Stream_Element_Array) return String is
--        ret    : String (1 .. item'length * 2);
--        cursor : natural := 1;
--        use type ada.Streams.Stream_Element;
--        hex    : constant array (ada.Streams.Stream_Element (0) .. ada.Streams.Stream_Element (15)) of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
--     begin
--        for i in item'range loop
--           ret (cursor) := hex (item (i) / 16);
--           cursor := cursor + 1;
--           ret (cursor) := hex (item (i) mod 16);
--           cursor := cursor + 1;
--        end loop;
--        return ret;
--     end;

   not overriding procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Ada.Streams.Stream_Element_Array)
   is
   begin
      This.setsockopt (Option,Value (Value'First)'Address, Value'Length);
   end setsockopt;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (This    : in out Socket;
      Address : String)
   is
      addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      ret  : Int;
   begin
      ret := Low_Level.zmq_connect (this.c, addr);
      Free (addr);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end Connect;

   ----------
   -- Send --
   ----------

   not overriding procedure Send
     (This    : in out Socket;
      Msg     : Messages.Message'Class;
      Flags   : Integer := 0)
   is
      ret  : Int;
   begin
      ret := Low_Level.zmq_send (this.c, msg.getImpl, int (Flags));
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : String;
                   Flags   : Integer := 0) is
   begin
      null;
   end;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Streams.Stream_Element_Array;
                   Flags   : Integer := 0) is
   begin
      null;
   end;
   not overriding
   procedure Send (This           : in out Socket;
                   Msg_Addres     : System.Address;
                   Msg_Length     : Natural;
                   Flags          : Integer := 0) is
   begin
      null;
   end;


   -----------
   -- flush --
   -----------

   not overriding procedure flush
     (This    : in out Socket)
   is
      ret  : Int;
   begin
      ret := Low_Level.zmq_flush (this.c);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end flush;

   ----------
   -- recv --
   ----------

   not overriding procedure recv
     (This    : in out Socket;
      Msg     : Messages.Message'Class;
      Flags   : Integer := 0)
   is
      ret  : Int;
   begin
      ret := Low_Level.zmq_recv (This.c,
                                 Msg.GetImpl,
                                 int (Flags));

      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end recv;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (this : in out Socket)
   is
      ret : int;
   begin
      if this.c /= Null_Address then
         ret := Low_Level.zmq_close (this.c);
         this.c := Null_Address;
         if ret /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
         end if;
      end if;
   end Finalize;


end ZMQ.Sockets;
