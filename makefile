.PHONY: liveView test

# deploy-hostname: build and deploy movement_detection on SD card
deploy-%:
	NAME=$* rebar3 grisp deploy -n movement_detection -v 1.0.0

# screen: show the screen of the grisp connected by usb3 cable
screen:
	sudo screen /dev/ttyUSB1 115200

# remote-hostname: open a remote shell connected to hostname
remote-%:
	erl -sname remote_$* -remsh movement_detection@$* -setcookie MyCookie -kernel net_ticktime 8

# shell: open a development shell (not a clean start)
shell:
	rebar3 as computer shell --sname movement_detection --setcookie MyCookie

# run_local: start movement_detection in release mode (clean start)
run_local:
	./_build/computer/rel/movement_detection/bin/movement_detection console

# local_release: build movement_detection in release mode for the computer
local_release:
	rebar3 as computer release

# start the visualization tool
liveView:
	@cd liveView/ && octave liveView.m

# run the unit tests
test:
	rebar3 eunit

# rm_logs: remove files generated when using the shell
rm_logs:
	@rm -f ./measures/*
	@rm -rf ./logs
	@rm -f ./rebar3.crashdump

# clean: remove build files
clean:
	@rm -rf ./_build

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Added by Nicolas Isenguerre in 2024, for ease of use.
# push: push the updated code for hera from another folder
# Call it with make push_hera COMMIT="yourcomment"
push_hera:
	./pushHera.sh "$(COMMIT)"

# Extended version of the 'clean' command. Also removes the _grisp folder for complete deletion of previous build.
clear:
	rm -rf ./_build
	rm -rf ./_grisp
	
