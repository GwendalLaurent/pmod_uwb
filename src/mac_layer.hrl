-include("pmod_uwb.hrl").

-define(FTYPE_BEACON, 3#000).
-define(FTYPE_DATA, 2#001).
-define(FTYPE_ACK, 2#010).
-define(FTYPE_MACCOM, 2#011).

-define(NONE, 2#00).
-define(SHORT_ADDR, 2#10).
-define(EXTENDED, 2#11).


-type ftype() :: ?FTYPE_BEACON | ?FTYPE_DATA | ?FTYPE_ACK | ?FTYPE_MACCOM.
-type addr_mode() :: ?NONE | ?SHORT_ADDR | ?EXTENDED.

% @doc frame control of a MAC header for IEEE 802.15.4
-record(frame_control, {frame_type = ?FTYPE_DATA :: ftype(),
                        sec_en = ?DISABLED :: flag(),
                        frame_pending= ?DISABLED :: flag(),
                        ack_req = ?DISABLED :: flag(),
                        pan_id_compr = ?DISABLED :: flag(),
                        dest_addr_mode = ?SHORT_ADDR :: addr_mode(),
                        frame_version = 2#00 :: integer(),
                        src_addr_mode = ?SHORT_ADDR :: addr_mode()}).

% @doc MAC header for IEEE 802.15.4
% Doesn't include the frame control nor a potential auxiliary security header
-record(mac_header, {seqnum = 0 :: integer(),
                     dest_pan = <<16#FFFF:16>> :: bitstring(),
                     dest_addr = <<16#FFFF:16>> :: bitstring(),
                     src_pan = <<16#FFFF:16>> :: bitstring(),
                     src_addr = <<16#FFFF:16>> :: bitstring()}).
