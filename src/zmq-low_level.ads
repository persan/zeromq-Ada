------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                         Z M Q . L O W _ L E V E L                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net            --
--                                                                          --
-- 0MQ Ada-binding is free software;  you can  redistribute it  and/or      --
-- modify it under terms of the  GNU General Public License as published    --
-- by the Free Soft-ware  Foundation;                                       --
-- either version 2,  or (at your option) any later version.                --
-- 0MQ Ada-binding is distributed in the hope that it will be useful, but   --
-- WITH OUT ANY WARRANTY;                                                   --
-- without even the  implied warranty of MERCHANTABILITY or                 --
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License    --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  The contents of this file is derived from zmq.h using the
--   -fdump-ada-spec switch for gcc.

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ.Low_Level is
   pragma Preelaborate;
   pragma Warnings (off);
   package defs is

      ZMQ_HAUSNUMERO : constant := 156384712;  --  zmq.h:56
      --  unsupported macro: EMTHREAD (ZMQ_HAUSNUMERO + 50)
      --  unsupported macro: EFSM (ZMQ_HAUSNUMERO + 51)
      --  unsupported macro: ENOCOMPATPROTO (ZMQ_HAUSNUMERO + 52)
      --  unsupported macro: ETERM (ZMQ_HAUSNUMERO + 53)

      ZMQ_MAX_VSM_SIZE : constant := 30;  --  zmq.h:107

      ZMQ_DELIMITER : constant := 31;  --  zmq.h:111
      ZMQ_VSM : constant := 32;  --  zmq.h:112

      ZMQ_MSG_MORE : constant := 1;  --  zmq.h:117
      ZMQ_MSG_SHARED : constant := 128;  --  zmq.h:118

      ZMQ_POLL : constant := 1;  --  zmq.h:148

      ZMQ_PAIR : constant := 0;  --  zmq.h:159
      ZMQ_PUB : constant := 1;  --  zmq.h:161
      ZMQ_SUB : constant := 2;  --  zmq.h:162
      ZMQ_REQ : constant := 3;  --  zmq.h:163
      ZMQ_REP : constant := 4;  --  zmq.h:164
      ZMQ_XREQ : constant := 5;  --  zmq.h:165
      ZMQ_XREP : constant := 6;  --  zmq.h:166
      ZMQ_UPSTREAM : constant := 7;  --  zmq.h:167
      ZMQ_DOWNSTREAM : constant := 8;  --  zmq.h:168

      ZMQ_HWM : constant := 1;  --  zmq.h:171
      ZMQ_SWAP : constant := 3;  --  zmq.h:173
      ZMQ_AFFINITY : constant := 4;  --  zmq.h:174
      ZMQ_IDENTITY : constant := 5;  --  zmq.h:175
      ZMQ_SUBSCRIBE : constant := 6;  --  zmq.h:176
      ZMQ_UNSUBSCRIBE : constant := 7;  --  zmq.h:177
      ZMQ_RATE : constant := 8;  --  zmq.h:178
      ZMQ_RECOVERY_IVL : constant := 9;  --  zmq.h:179
      ZMQ_MCAST_LOOP : constant := 10;  --  zmq.h:180
      ZMQ_SNDBUF : constant := 11;  --  zmq.h:181
      ZMQ_RCVBUF : constant := 12;  --  zmq.h:182
      ZMQ_RCVMORE : constant := 13;  --  zmq.h:183

      ZMQ_NOBLOCK : constant := 1;  --  zmq.h:186
      ZMQ_SNDMORE : constant := 2;  --  zmq.h:187

      ZMQ_MORE : constant := 2;  --  zmq.h:189

      ZMQ_POLLIN : constant := 1;  --  zmq.h:206
      ZMQ_POLLOUT : constant := 2;  --  zmq.h:207
      ZMQ_POLLERR : constant := 4;  --  zmq.h:208

      ZMQ_STREAMER : constant := 1;  --  zmq.h:228
      ZMQ_FORWARDER : constant := 2;  --  zmq.h:229
      ZMQ_QUEUE : constant := 3;  --  zmq.h:230

   end defs;

   procedure Zmq_Version (Major : not null access int; Minor : not null access int; Patch : not null access int);  -- zmq.h:48
   pragma Import (C, Zmq_Version, "zmq_version");

   function Zmq_Strerror (Errnum : int) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:82:48
   pragma Import (C, Zmq_Strerror, "zmq_strerror");

   type zmq_msg_t_vsm_data_array is array (0 .. 29) of aliased unsigned_char;
   type zmq_msg_t is record
      content  : System.Address;  -- zmq.h:106:11
      shared   : aliased unsigned_char;  -- zmq.h:107:19
      vsm_size : aliased unsigned_char;  -- zmq.h:108:19
      vsm_data : aliased zmq_msg_t_vsm_data_array;  -- zmq.h:109:45
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_msg_t);  -- zmq.h:110:3

   --  skipped anonymous struct anon_0

   --  skipped function type zmq_free_fn

   function zmq_msg_init (msg : access zmq_msg_t) return int;  -- zmq.h:114:44
   pragma Import (C, zmq_msg_init, "zmq_msg_init");

   function zmq_msg_init_size (msg : access zmq_msg_t; size : Natural) return int;  -- zmq.h:115:62
   pragma Import (C, zmq_msg_init_size, "zmq_msg_init_size");

   function zmq_msg_init_data
     (msg  : access zmq_msg_t;
      data : System.Address;
      size : Natural;
      ffn  : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint : System.Address) return int;  -- zmq.h:117:46
   pragma Import (C, zmq_msg_init_data, "zmq_msg_init_data");

   function zmq_msg_close (msg : access zmq_msg_t) return int;  -- zmq.h:118:45
   pragma Import (C, zmq_msg_close, "zmq_msg_close");

   function zmq_msg_move (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:119:61
   pragma Import (C, zmq_msg_move, "zmq_msg_move");

   function zmq_msg_copy (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:120:61
   pragma Import (C, zmq_msg_copy, "zmq_msg_copy");

   function zmq_msg_data (msg : access zmq_msg_t) return System.Address;  -- zmq.h:121:46
   pragma Import (C, zmq_msg_data, "zmq_msg_data");

   function zmq_msg_size (msg : access zmq_msg_t) return Natural;  -- zmq.h:122:47
   pragma Import (C, zmq_msg_size, "zmq_msg_size");

   function zmq_init
     (app_threads : int) return System.Address;  -- zmq.h:130:70
   pragma Import (C, zmq_init, "zmq_init");

   function zmq_term (context : System.Address) return int;  -- zmq.h:131:39
   pragma Import (C, zmq_term, "zmq_term");

   function zmq_socket (context : System.Address; c_type : int) return System.Address;  -- zmq.h:176:53
   pragma Import (C, zmq_socket, "zmq_socket");

   function zmq_close (s : System.Address) return int;  -- zmq.h:177:34
   pragma Import (C, zmq_close, "zmq_close");

   function zmq_setsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : Natural) return int;  -- zmq.h:179:21
   pragma Import (C, zmq_setsockopt, "zmq_setsockopt");

   function zmq_getsockopt
     (arg1 : System.Address;
      arg2 : int;
      arg3 : System.Address;
      arg4 : access Natural) return int;  -- zmq.h:195:16

   pragma Import (C, zmq_getsockopt, "zmq_getsockopt");

   function zmq_bind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:180:51
   pragma Import (C, zmq_bind, "zmq_bind");

   function zmq_connect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:181:54
   pragma Import (C, zmq_connect, "zmq_connect");

   function zmq_send
     (s     : System.Address;
      msg   : access zmq_msg_t;
      flags : int) return int;  -- zmq.h:182:60
   pragma Import (C, zmq_send, "zmq_send");

   function zmq_recv
     (s     : System.Address;
      msg   : access zmq_msg_t;
      flags : int) return int;  -- zmq.h:184:60
   pragma Import (C, zmq_recv, "zmq_recv");

   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:195:11
      fd      : aliased int;  -- zmq.h:199:9
      events  : aliased short;  -- zmq.h:201:11
      revents : aliased short;  -- zmq.h:202:11
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_pollitem_t);  -- zmq.h:203:3

   --  skipped anonymous struct anon_1

   function zmq_poll
     (items   : access zmq_pollitem_t;
      nitems  : int;
      timeout : long) return int;  -- zmq.h:205:73
   pragma Import (C, zmq_poll, "zmq_poll");

   function zmq_device
     (arg1 : int;
      arg2 : System.Address;
      arg3 : System.Address) return int;  -- zmq.h:232:16
   pragma Import (C, zmq_device, "zmq_device");


   function zmq_stopwatch_start return System.Address;  -- zmq.h:215:39
   pragma Import (C, zmq_stopwatch_start, "zmq_stopwatch_start");

   function zmq_stopwatch_stop (watch_u : System.Address) return unsigned_long;  -- zmq.h:219:58
   pragma Import (C, zmq_stopwatch_stop, "zmq_stopwatch_stop");

   procedure zmq_sleep (seconds_u : int);  -- zmq.h:222:40
   pragma Import (C, zmq_sleep, "zmq_sleep");

end ZMQ.Low_Level;
