-include("pmod_uwb.hrl").

-define(FTYPE_BEACON, 3#000).
-define(FTYPE_DATA, 2#001).
-define(FTYPE_ACK, 2#010).
-define(FTYPE_MACCOM, 2#011).

-define(NONE, 2#00).
-define(SHORT_ADDR, 2#10).
-define(EXTENDED, 2#11).


%--- Types ---------------------------------------------------------------------
-export_type([ftype/0, addr_mode/0, addr/0, frame/0, beacon_metadatas/0]).
-type ftype() :: ?FTYPE_BEACON | ?FTYPE_DATA | ?FTYPE_ACK | ?FTYPE_MACCOM.
-type addr_mode() :: ?NONE | ?SHORT_ADDR | ?EXTENDED.
-type addr() :: bitstring().

-type frame() :: {frame_control(), mac_header(), bitstring()}.

-type beacon_metadatas() :: {superframe_specs(), gts_fields(), pending_addr_flds()}.
%--- Records -------------------------------------------------------------------
-export_type([frame_control/0, mac_header/0, superframe_specs/0, gts_fields/0, gts_descr/0]).

% @doc frame control of a MAC header for IEEE 802.15.4
-record(frame_control, {frame_type = ?FTYPE_DATA :: ftype(),
                        sec_en = ?DISABLED :: flag(),
                        frame_pending= ?DISABLED :: flag(),
                        ack_req = ?DISABLED :: flag(),
                        pan_id_compr = ?DISABLED :: flag(),
                        dest_addr_mode = ?SHORT_ADDR :: addr_mode(),
                        frame_version = 2#00 :: integer(),
                        src_addr_mode = ?SHORT_ADDR :: addr_mode()}).

-type frame_control() :: #frame_control{}.

% @doc MAC header for IEEE 802.15.4
% Doesn't include the frame control nor a potential auxiliary security header
-record(mac_header, {seqnum = 0 :: non_neg_integer(),
                     dest_pan = <<16#FFFF:16>> :: addr(),
                     dest_addr = <<16#FFFF:16>> :: addr(),
                     src_pan = <<16#FFFF:16>> :: addr(),
                     src_addr = <<16#FFFF:16>> :: addr()}).

-type mac_header() :: #mac_header{}.

% Sec. 5.2.2.1.2 p.62
-record(superframe_specs, {beacon_order     :: integer(),
                           superframe_order :: integer(),
                           final_cap_slot   :: integer(),
                           ble              :: boolean(),
                           pan_coord        :: boolean(),
                           association_perm :: boolean()}).

-type superframe_specs() :: #superframe_specs{}.

-record(gts_descr, {short_addr    :: <<_:16>>,
                    starting_slot :: non_neg_integer(),
                    gts_length    :: non_neg_integer()}).

-type gts_descr() :: #gts_descr{}.

-record(gts_fields, {gts_descr_cnt :: integer(),
                     gts_permit    :: boolean(),
                     gts_direction  :: bitstring(),
                     gts_descr_list :: [gts_descr()]}).

-type gts_fields() :: #gts_fields{}. 

-record(pending_addr_flds, {nbr_short_addr_pending :: non_neg_integer(),
                              nbr_ext_addr_pending   :: non_neg_integer(),
                              short_addr_pending     :: [<<_:16>>],
                              ext_addr_pending       :: [<<_:64>>]}). % TODO

-type pending_addr_flds() :: #pending_addr_flds{}.
