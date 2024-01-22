initiator: mount compile
	sed -i 's/static ip_address=192\.168\.2\.7./static ip_address=192\.168\.2\.75/' grisp/default/common/deploy/files/dhcpcd.conf
	NODE_NAME=uwb_initiator rebar3 grisp deploy

responder: mount compile
	sed -i 's/static ip_address=192\.168\.2\.7./static ip_address=192\.168\.2\.74/' grisp/default/common/deploy/files/dhcpcd.conf
	NODE_NAME=uwb_responder rebar3 grisp deploy

mount:
	mountpoint -q  /run/media/michel/GRISP_SD || udisksctl mount -b /dev/sdg1

compile:
	rebar3 compile

make: mount compile
	rebar3 grisp deploy
