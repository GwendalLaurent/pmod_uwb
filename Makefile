SD_CARD_LOC := /Volumes/GRISP_SD


initiator: mount compile
	sed -i '' 's/static ip_address=192\.168\.2\.7./static ip_address=192\.168\.2\.75/' grisp/default/common/deploy/files/dhcpcd.conf
	rebar3 as initiator grisp deploy

responder: mount compile
	sed -i '' 's/static ip_address=192\.168\.2\.7./static ip_address=192\.168\.2\.74/' grisp/default/common/deploy/files/dhcpcd.conf
	rebar3 as responder grisp deploy

coordinator: mount compile
	sed -i '' 's/static ip_address=192\.168\.2\.7./static ip_address=192\.168\.2\.70/' grisp/default/common/deploy/files/dhcpcd.conf
	rebar3 as coordinator grisp deploy

mount:
	@if [ $$(diskutil info $(SD_CARD_LOC) > /dev/null 2>&1; echo $$?) -ne 0 ]; then \
		echo "Disk is not mounted. Mounting now..."; \
		diskutil mount $(SD_CARD_LOC); \
	else \
		echo "Disk is already mounted."; \
	fi
compile:
	rebar3 compile

make: mount compile
	rebar3 grisp deploy

dialyzer:
	rebar3 dialyzer -u -s

tests:
	rebar3 ct --sname test
