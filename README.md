&emsp; NOTE: this README is the exact copy of Appendix E of the final thesis .pdf, which can be found by clicking [here](https://github.com/Neackow/movement_detection/blob/main/ISENGUERRE_50041800_2024.pdf). Do not hesitate to ask Pr. Peter Van Roy for the link to the official file from the dial.mem library. Also note that some files were deleted in this version of the project. To see the deleted files, refer to the previous theses: 
  - Lucas Nélis' version (numerl developper): [https://github.com/lunelis/sensor_fusion](https://github.com/lunelis/sensor_fusion) ;
  - Hera2.0: the original version, by Sébastien Kalbusch and Vincent Verpoten: [https://github.com/sebkm/sensor_fusion](https://github.com/sebkm/sensor_fusion) ;

&emsp; **BEWARE**: these versions are outdated when it comes to this thesis. All the necessary files are stored in this repository)

# Updated user manual

&emsp; In previous theses, the authors have created and updated a user manual detailing how to use their application, whilst also globally explaining how to start up with the GRiSP board and Erlang. However, it was never updated for the GRiSP2 board.

&emsp; The author encountered numerous problems, which were solved one by one over the course of several weeks. To avoid future generations of Erlang programmers struggling with the GRiSP environment, the author here proposes a guide on how to install Erlang, Hera, etc., followed by a user manual explaining how the application developed in this thesis can be used.

&emsp; Both of these are heavily inspired by the previous user manual, which can be found [here](https://github.com/lunelis/sensor_fusion). This user manual is also a great source of information concerning the older versions of the system.

## How to install Erlang, Hera, ...

&emsp; Each part starts with the “go-to” procedure to complete the installation. An erratum and summary of problems faced at each step by the author can be found in the **_Struggles faced_** part of each section.

### ** Disclaimer **

&emsp; The following tutorial was done on a computer running on Linux-Ubuntu 20.04, installed in February 2024. The author cannot guarantee it will work for newer versions (though it should) or for other OS (e.g.: do not even try to use wsl on Windows. There are a lot of dependencies that do not exist in wsl but that are required to make this whole thing work. Linux is the go-to OS for the GRiSP environment), nor does he guarantee that updated versions of the packages required to run Erlang and compile it will work with GRiSP. This tutorial, furthermore, is not as detailed as one would hope it to be: the installation process took place in early February, whilst the writing of this section was done in late May. If any questions emerge, the author will gladly help future users with their struggles. Note: another great source of help is the GRiSP [Slack channel](https://github.com/grisp/grisp/wiki).


&emsp; Some parts of this tutorial are directly inspired by the GRiSP wiki. Refer to this [website](https://github.com/grisp/grisp/wiki) (GRiSP Wiki).


#### Required hardware

&emsp; For this tutorial, one needs:
  - A computer running on Linux;
  - One Wi-Fi access point: it is recommended to use a smartphone (or computer) as a hotspot, since the Eduroam network does not really like connecting on prototyping boards (for safety reasons);
  - One or more GRiSP2 boards (with SD-card);
  - Pmod™ sensors;

&emsp; You can find all the GRiSP related hardware at [https://www.grisp.org/shop/](https://www.grisp.org/shop/).

#### Required software

&emsp; To use the system, one needs to have installed on its computer:
  - Erlang/OTP 25.0, Erts 13.2;
  - rebar3 3.22.1;
  - rebar3 hex;
  - rebar3 grisp;

&emsp; The following explains in more detail how to install these.

### Setting up a development environment

#### Installing Erlang

&emsp; It is recommended to use a version manager to get your **Erlang installation**. _asdf_ or _kerl_ are
fine. In this tutorial, _kerl_ will be explained.

&emsp; The first thing to do is to follow [this tutorial](https://github.com/kerl/kerl)
to install _kerl_. If you are using a new computer,
many packages need to be installed in parallel. A list of such packages can be found [here](https://github.com/asdf-vm/asdf-erlang/pull/10/files). Note
that this is technically for _asdf_, but the same goes for _kerl_. Moreover, the package “_wxWidget_” did
not work for the author. Next, install Erlang itself using _kerl_. Then, it is recommended to add the
installation’s activation to the system’s PATH. A tutorial on how to add something to the PATH
can be found in this [website](https://www.howtogeek.com/658904/how-to-add-a-directory-to-your-path-in-linux/). Be careful: if you modify directly the _bashrc_ file, it uses different
commands from those which can be used in a Linux shell.

#### _Struggles faced_

&emsp; Upon trying : “kerl install 25.3 /usr/local/lib/erlang/25.3”, an “**Access Denied**” message appeared. This was due to the fact that the user profile on the computer did not have access to the
directories mentioned. For any directory to which access is denied but is required, in the shell, go to said directory and type:

``` bash 
sudo chmod 777 directoryName
```

This will allow to read, write and execute on the directory for any user on the computer. If the
error message happens on any sub-directory, do the same on it. Beware: it is ugly and can be
dangerous. Anyone on your computer can now modify your Erlang installation. But, seriously,
who would?

&emsp; The rest of the installation process should go fine. You now have Erlang installed on your
computer.

#### rebar3

&emsp; This is required to build Erlang projects. Follow the “getting started” tutorial on this [web page](https://github.com/erlang/rebar3).

#### _Struggles faced_

&emsp; At the “_cd rebar3_” followed by “_./bootstrap_” step, the following error message appeared:

``` bash
OTP Application crypto not available. Please fix your Erlang install to support it and try again .
```

This happened because the author installed ssl (ssh command in earlier steps) _after_ installing
Erlang. Do not do that. If you did, delete the installation then build and redo with all packages
installed. Once it is done, “_./bootstrap_” should work better (green lines with “===>” at the
start). You can then proceed with “_./rebar3 local install_”.

#### GRiSP Rebar3 plugin

&emsp; In the previous user manual, it says to add the plugin configuration for GRiSP in the file
“_/.config/rebar3/rebar.config_”... Which did not exist. Create the directory and “_rebar.config_” file
and then add the following plugin configuration (Note: this is not the same as in the previous user manual. Indeed, it struggled to load the plugins. This update
version details where to find the files, which helps the process):

``` erlang
{plugins, [
    {rebar3_hex, 
        {git, "https://github.com/erlef/rebar3_hex.git", 
            {branch, "main"}
        }
    },
    {rebar3_grisp, 
        {git, "https://github.com/grisp/rebar3_grisp.git", 
            {branch, "master"}
        }
    }
]}.
```
If you get a “no escript” error message, it is because the system detects no active Erlang/OTP
installations. Make sure to have an active one in the shell in which you are doing the installation
procedures.

&emsp; Some notes at this point:
  - To find a command which you already typed earlier rapidly, in the shell, type: “_ctrl+r_” and
the beginning of the command;
  - Do not forget that in order to run rebar3, etc., you should always have an active and valid
Erlang session running. Typically, every time you start a shell, type:

``` bash
. /usr/local/lib/erlang/25.3/activate
```

The version being whatever version you have. You can also add rebar3 to your PATH
(recommended, at each shell launch, in order to have the rebar3 command work everywhere),
using:

``` bash
export PATH=\$PATH:$\sim$/.cache/rebar3/bin
```

&emsp; Typically, the author would start each new shell by using “_ctrl+r_” and typing the beginning
of one of the commands. Do not hesitate to put them one after the other using “_&&_” so that
everything is done in one go.

&emsp; Lastly, to verify that rebar3 is correctly installed (and to update the plugins if need be), you
can type the following in the shell:

``` bash 
rebar3 update && rebar3 plugins list
```

#### First tests

&emsp; With that, everything should be good regarding Erlang! You can try the tutorials given by the
GRiSP team (to create: [https://github.com/grisp/grisp/wiki/Creating-Your-First-GRiSP-Application](https://github.com/grisp/grisp/wiki/Creating-Your-First-GRiSP-Application) and deploy: [https://github.com/grisp/grisp/wiki/Deploying-a-GRiSP-Application](https://github.com/grisp/grisp/wiki/Deploying-a-GRiSP-Application) the application) that flashes the LED. To create a new app, in the rebar3 folder, type:

``` bash
rebar3 new grispapp name=Name dest=/whereisyourSDcard
```
e.g.
``` bash
rebar3 new grisapp name=Whatever dest=/media/nicolas/GRISP
```

&emsp; This creates a folder in rebar3. You can take this folder wherever you want, the commands for
rebar3 will work if you added rebar3 to your PATH.

### Connection over serial

&emsp; To open the Erlang shell on your computer to directly work on the GRiSP2, use:

``` bash
sudo picocom /dev/ttyUSB1 --baud 115200 --echo
```

in your application’s folder. To leave picocom without cancelling the program (which is achieved
by “_q()._” in the Erlang shell), use “_ctrl+a_” followed by “_ctrl+q_”. Note that this only works for
tty, as when you use a remote shell (see later), type first “_q()._” then “_ctrl+c_”.

### The Wi-Fi and connecting remotely

&emsp; The author recommends that you to follow this tutorial from scratch in order to better understand what is happening. Start from a random project, typically the one which you created to do
the LED example. Important note: you only need to have the IP address of the boards if you wish
to connect to them remotely. Otherwise, simply doing the “_wpa_supplicant.conf_ ” part should be
enough

&emsp; First, follow the GRiSP [tutorial](https://github.com/grisp/grisp/wiki/Connecting-over-WiFI-and-Ethernet) up until “Finding out the IP address of the GRiSP board
when using DHCP”, at which point you could face some problems, as the board maybe never
connected to the Wi-Fi beforehand. Jump to the **GRiSP INI** section, follow it, and then follow
**Configuring Wi-Fi**. Next, deploy the application on your SD-card as per usual. Put the SD-card
in the board and supply it. You should see messages in the shell about _wlan0_ mentioning errors
(_File exists_). This is not a problem. To find the IP-address of the GRiSP2 board, type:

``` bash
inet:getifaddrs().
```
&emsp; Go back to the tutorial to complete everything regarding the “_erl_inetrc_” file and add the IP-address of the board to your host file by typing, in a normal shell:

``` bash
sudo vi /etc/hosts
```
&emsp; For example, a completed “_/etc/hosts_” file would look like:
``` bash 
127.0.1.1       hostname_computer
192.168.43.215  board_1
192.168.43.6    board_2
```
and a completed “_erl_inetrc_” would look like:
``` erlang 
{host, {192,168,43,32}, ["neackow_z4"]}.
{host, {192,168,43,215}, ["nav_1"]}.
{host, {192,168,43,6}, ["orderCrate"]}.
```

&emsp; You can now connect to the GRiSP2 remotely using:
``` bash 
erl -sname my_remote_shell -remsh my_project@my_grisp_board -setcookie MyCookie
```
and replace everything with the names you have given to your project, etc.

&emsp; Be careful: connecting remotely to the GRiSP2 board does not connect directly to the board
itself: it creates a distributed Erlang node solely for the purpose of doing a remote shell. This
means that there is an intermediate node between the computer and the GRiSP2 board.

&emsp; Also, all this is already done in the applications files (More on how to change the applications file to adapt to your computer later). All you have to do is to adapt the files
to your computer’s name, IP address, etc. Moreover, in the applications directory, one can use the
“_makefile_” to connect remotely, using:
``` bash 
make deploy-NameOfBoard
```
&emsp; Be careful: the board’s name is **very important** since it dictates the behaviour of the board,
as has been explained in previous chapters. Note that this command takes the name of the board
as a variable value and attributes it to the “_env.NAME_” variable in the “_grisp.ini.mustache_” file.

&emsp; Some notes:
  - The author tried to be smart and put his computer node twice in the “erl-inetrc” file, so
he would not have to care about whether he was at home or not. It seems like it does
not like having twice the same name, however. Comment the node which you do not need
and rebuild. Note: this only applies if you use different Wi-Fi depending on where you are
working. Again, it is advised to use your smartphone as a Wi-Fi hotspot, since it will _always_
give the boards, computers, etc. the same IP address.
  - Obviously, make sure the Wi-Fi network that the GRiSP2 board is trying to reach is not
disabled.
  -  Beware of syntax error in the “_erl_inetrc_” file. Instead of separating the numbers by a “,”
one could type the IP address with “.”, but this does not work.
  - If you wish to connect remotely to a node, the author believes that the computer should be
on the same Wi-Fi as the boards, as he got a “*** ERROR: Shell process terminated! (ˆG
to start new job) ***” error if not connected on the same network. This only applies when
you try to connect remotely.
  - Using your smartphone as a hotspot can lead to unexpected behaviours. Often, the GRiSP2
board would not manage to connect to the smartphone’s Wi-Fi, despite the computer being
able to. Or if it did, it would, out of nowhere, disconnect and never find the Wi-Fi ever
again. It was found that this was due to the “Saving energy” mode of the phone, which
forbids/periodically interrupts any remote connection from GRiSP2 boards to its hotspot.
Upon deactivating it, the boards would find the Wi-Fi (almost) all the time. It was also
observed that the ”Silent” mode of the phone could disturb the connection.

#### _Struggles faced_

&emsp; Due to the **numerl** NIF, the author never managed to run the Erlang application on his computer.
Hereafter are some troubles he faced while trying to do so.

&emsp; Using “make shell” worked, but the clean start “make local_release && make run_local” did not,
with error: “**Could not start kernel pid, application controller, invalid config data: application:
grisp; duplicate parameter: devices**”. In the author’s “_home/nicolas/TFE/sensor_fusion/_build/
computer/rel/sensor_fusion/releases/1.0.0/_” folder, several files were available. In “_sys.config_”,
two {_devices_} variables were set. These variables were coming from the “_computer.config.src_” file
in the “_config_” folder. The author tried to comment one of the two and rebuild.

&emsp; It now said that it did not find “_numerl.so_”. The error pointed to “_sensor_fusion/src/numerl.erl,
line 9_”. Upon analysing, it was found out that line 9 calls “_erlang:load_nif_”, which takes as
first argument the file path to the shareable object/dynamic library, minus the OS-dependent
extension (.so in Linux), so here, _numerl_. However, “_numerl.so_” seems to be moved in the trash,
as was found out by using the command “find /home -name numerl.so”. So there was a problem
here, as it is necessary that the system accesses this file to correctly compile the whole project.
According to this [wiki](https://github.com/grisp/grisp/wiki/NIF-Support), there are some special requirements, such as having a main C file named
“_NAMEOFDRIVER_nif.c_” and this file must reside in the application’s top level folder, under the
following path:
``` bash
grisp/$<$platform$>$/$<$version$>$/build/nifs/NAMEOFDRIVER_nif.c
```
In the current case, the author tried to following path:
``` bash
grisp/grisp2/default/build/nifs/numerl_nif.c
```
Initially, the file was located under:
``` bash 
grisp/grisp2/common/build/nifs/numerl_nif.c
```
But this attempt in changing the file from folders returned an even weirder error. Upon rebuilding
the project, the following message appeared:
``` bash 
* Patching
    [grisp] 00100-rtems.patch (already applied, skipping)
    [grisp] 00300-drivers-nifs.patch ===> sh(git apply 
--ignore-whitespace 00300-drivers-nifs.patch) 
%% Note: this file can be found in 
%% _grisp/grisp2/otp/<your_version>/build/
failed with return code 1 and the following output:
error: patch failed: erts/emulator/Makefile.in:938 
%% At this line, we find: 
%% ASMJIT_PCH_OBJ=$(TTF_DIR)/asmjit/asmjit.hpp.gch
error: erts/emulator/Makefile.in: patch does not apply
```
where lines starting with “_%%_” indicate a comment from the author.

&emsp; In the face of this error message, the author realised that either his method was wrong, or there
was something beyond his understanding. So, he tried putting the aforementioned file to where it
previously was. Upon trying to build once more, the following message appeared in the shell:
``` bash
===> Preparing
* Patching
    [grisp] 00100-rtems.patch (already applied, skipping)
    [grisp] 00300-drivers-nifs.patch (already applied, skipping)
* Copying files
    [grisp] erts/emulator/sys/unix/erl\_main.c
    [grisp] xcomp/erl-xcomp-arm-rtems5-25.conf
    [grisp] xcomp/erl-xcomp-arm-rtems5.conf
* Copying drivers
    [grisp] erts/emulator/drivers/unix/grisp\_termios_drv.c
* Copying nifs
    [grisp] erts/emulator/nifs/common/grisp\_gpio_nif.c
    [grisp] erts/emulator/nifs/common/grisp\_hw\_nif.c
    [grisp] erts/emulator/nifs/common/grisp\_i2c\_nif.c
    [grisp] erts/emulator/nifs/common/grisp\_rtems\_nif.c
    [grisp] erts/emulator/nifs/common/grisp\_spi\_nif.c
    [sensor_fusion] erts/emulator/nifs/common/numerl\_nif.c 
    %% SO apparently, it DOES copy it. So the question is: 
    %% why is it not available?
```
&emsp; As the author later realised that he did not need to launch the application on his computer, he
gave up on the matter. This problem is thus left with no answer.

&emsp; Note about “_make local_release && make run_local_”: the problem is that “_numerl.so_” is not
loading, and apparently, according to Sébastien Kalbusch (one of the two Hera2.0 developers), it
is normal and if one wanted to have the possibility to run the code directly on the shell, the NIF
should be discarded on the computer. It is to be noted that the only advantage of having the
application run on the computer is to have the logs directly appearing on it without having to
remove the SD card at each test.

### Installing the Arduino IDE

&emsp; The author would like to point out that this section refers a lot to the ESP32, as it was the first
micro-controller that he attempted to use. However, every manipulation done here will be useful
for the Raspberry Pi Pico W.

&emsp; The Arduino IDE has been used to code the C++ code of the micro-controller. Another way
of coding in Arduino but in _VSCode_ is to use _Platformio_, but the author does not especially
recommend it, as it seemed unreliable on his computer. It worked (vaguely), but was not really
intuitive nor fast to deploy the code on the board.

&emsp; First, install the Arduino IDE itself by following the tutorial you can find [here](https://docs.arduino.cc/software/ide-v2/tutorials/getting-started/ide-v2-downloading-and-installing/). Next, upon
following this [tutorial](https://bromleysat.com/installing-the-esp32-board-in-arduino-ide), the author faced several difficulties:
  - The _PySerial_ package was not installed on the computer. Follow this [tutorial](https://www.geeksforgeeks.org/how-to-install-python-serial-package-on-linux/) to install
it;
  - He had some struggles with the ports not being detected (typically, _dev/ttyUSB0_): following
what was said on this [page](https://askubuntu.com/questions/133235/how-do-i-allow-non-root-access-to-ttyusb0), by using the part mentioning the following command:
``` bash
sudo usermod -a -G dialout $USER
```
After restarting the computer, the problem was solved.
  - One could face port problems if the appropriate driver (UART-USB CP120X) is not installed.
On Ubuntu, the driver should be installed by default. If not, you can follow this [tutorial](https://askubuntu.com/questions/941594/installing-cp210x-driver);

### Installing the application

&emsp;Go to the git repository ([here](https://github.com/grisp/grisp/wiki/Building-the-VM-from-source)). Clone the git wherever you want it to be on your computer.
_hera_ and _hera_synchronization_ are added using the build files, like “_rebar.config_”. Technically, you
do not need to install them on your computer in order for the whole application to work.
&emsp;This version of the application uses **numerl**, a custom NIF for fast matrix operations. To use
the **numerl** NIF, you must first compile a custom version of OTP. This can be achieved using a
Docker or a toolchain. The previous user manual says to follow this [tutorial](https://github.com/grisp/grisp/wiki/Building-the-VM-from-source) and install a Docker
by adding the following line to the “_grisp/build/toolchain_” section of “_rebar.config_”:
``` erlang 
{grisp, [
    {build, [
        {toolchain, [
            {docker, "grisp/grisp2-rtems-toolchain"}
        ]}
    ]}
]}
```
and then run the command:
``` bash
rebar3 grisp build --docker
```
which should generate a “_grisp” folder containing the VM running on the GRiSP. However...

#### _Struggles faced_

&emsp; The build was not working. It appears that rebar3 does not know what Docker is, despite having
installed it. To install Docker, see this [link](https://docs.docker.com/engine/install/ubuntu/). After basically one hour of scanning the Internet,
the author went on the Docker hub website, into the “_grisp/grisp2-rtems-toolchain_” and did the
following Docker pull:
``` bash
sudo docker pull grisp/grisp2-rtems-toolchain
```
The goal was to have the Docker image locally. Doing this without sudo failed (“**daemon socket
connection error**”), but this sadly did not correct the problem.

&emsp;To get the Docker running, on this [page](https://github.com/grisp/rebar3_grisp/releases), they mention adding the Docker toolchain possibility.
Downloading the .zip, in one of the files of the “_src/_” directory, the author saw that the build was
trying to ask for “_docker info_”, which returned an error, leading to the problem. To avoid this,
he followed the advice given on this [page](https://phoenixnap.com/kb/cannot-connect-to-the-docker-daemon-error). This finally resulted in a functioning Docker. But
next, after putting the right Erlang/OTP version in the “_rebar.config_” file, the author reached a
new error, saying it did not find a C compiler for the code, despite having “_gcc_” installed on the
computer.
&emsp;Despite best efforts, he could not make the Docker work. Instead:

#### Installing a toolchain

&emsp;Go to this [page](https://github.com/grisp/grisp2-rtems-toolchain) and “git clone” the repository, then type, in your shell:
```bash
make install
```
within the repository. This will take a while. A very long while (almost an hour, in this case). Then, in “_rebar.config_”, instead of
```erlang
{docker, "grisp/grisp2-rtems-toolchain"}
```
put:
```erlang
{directory, "where_is_your_toolchain_installed_on_your_computer"}
```
e.g., for the author:
```erlang
{directory, "/home/nicolas/TFE/grisp2-rtems-toolchain/rtems/5"}
```
&emsp;You should now be able to compile the application. Go to the directory and type “_rebar3 grisp build_”. Again, the build will take a while.

#### Having your own Hera repositories

&emsp;In order to modify the code of Hera and debug the application, you need to create your own
repositories. Here, step by step, is how to do it:
  - Go on your GitHub account;
  - Create the repositories: the author recommends keeping “_hera_” and “_hera_synchronization_”
as default names for these. However, you can call your main repository whatever you want
(in this project, it was named “_movement_detection_”);
  - Locally clone all the newly created repositories, then download the original repositories’
content from the author’s git (download the .zip and paste everything in the repository, it is
easier). Place everything inside the correct folder;
  - Then, in the shell, for each local folder:
``` bash
git add * 
git commit -m "My amazing first comment"
git push -u origin main
```
  - Once the repositories are set up, you can now go and modify “_rebar.config_” to put your names
and reference to your repositories in the {_deps_} part, at the beginning of the file. You will
have to change things in the “_hera_” repository and in the main repository. There is nothing
to change in “_hera_synchronization_”;
  - Then:
    - Push every changes;
    - Delete “__grisp_” and “__build_” in the main folder, if they appear;
    - Do “_rebar3 grisp build_” in the shell within the application’s main folder;
  And you should now have an application fully built with your repositories.

### Other notes

-  If your SD-card goes into “read only mode”, it is because the little lid on the side of the micro-SD −→ SD-card converter is put on “locked”. Simply unlock it as shown [here](https://askubuntu.com/questions/213889/microsd-card-is-set-to-read-only-state-how-can-i-write-data-on-it).
 - The author reminds the reader that if he faces any problems, the GRiSP team is very reactive on the GRiSP Slack

## User Manual: the _movement_detection_ application

### Adapting the configuration files to your system

&emsp;There are three files in the “_/config_” folder:

#### _computer.config.src_

&emsp;The only thing you need to change is within the {_sync_node_optional_} part, where you need to
put your boards’ name and your computer’s name. Note: your computer’s name is here required
only if you are intent on launching the application on it, and use the computer as a node of the
network. E.g.:
```erlang
{sync_nodes_optional, [
    movement_detection@neackow_z4,
    movement_detection@nav_1,
    movement_detection@orderCrate
]},
```
&emsp;Be careful: if you have “-” in your computer’s name, the deployment of the application will not
work. To change your computer’s name on Linux:
  - On the top left corner, go to “_Activities_”;
  - Search for “_About_”;
  - The first thing will be the computer name: you can change it to whatever, without a “-” or
arithmetic sign.

&emsp;Note: if you add any “_hera_” environment variables and that you want them to affect your
computer, add them in the {_hera_} part of the file. E.g.:
```erlang
{hera, [
    {log_data, false},
    {show_log, false},
    {show_log_spec,false},
    {log_BL,false}
]},
```

#### _sys.config_

&emsp;Here, again, change the {_sync_node_optional_} part. Nothing more is required.

#### _vm.args_

&emsp;This sets the arguments of the Virtual Machine. You can set a new Cookie name (be careful to
then change it every where) in the “**-setcookie**” line, or the name of the deployed application in
the “**-sname**” line.
&emsp;Next, if you followed everything that was mentioned previously, there is only one thing left to
do in the “_rebar.config_” file, which contains all the information to build and deploy the system:
tell the system the path to the SD-card on which you wish to deploy the application. Typically,
for the author:
```erlang
{deploy , [
    {pre_script, "rm -rf /media/nicolas/GRISP/*"},
    {destination, "/media/nicolas/GRISP"},
    {post_script, "umount /media/nicolas/GRISP"}
]}
```

#### Within the _/src_ folder

&emsp; The only file that requires your attention is “_movement_detection.app.src_”. If you added any
modules, it is required to add them to the {_modules_} part.

#### Files for networking

&emsp; These files can be found in the “_grisp/grisp2/common/deploy/files/_” folder. If you followed the
previous steps, “_erl_inetrc_” and “_wpa_supplicant.conf_ ” should already be adapted to your system
and Wi-Fi networks. Note: in “_grisp.ini.mustache_”, at the end of the second line, you can change
the name of the Erlang cookie if you desire.

#### For the Hera application

&emsp; If you did not created your own Hera repository, this step is not useful. If you did, here are some
notes about it:
  - In “_ebin/hera.app_”, you need to specify the {_env_} variables and every new module that you
add to Hera. The same goes for “_src/hera.app.src_”.
  - In “_rebar.config_”, do not forget to change the git reference to “_hera_synchronization_”.

### How to properly build and deploy the application

&emsp; Start by removing any “_rebar.lock_” files, either by renaming them or completely removing them.
Check that absolutely **no** “_rebar.lock_” are left, either in your main application folder or in Hera
folders. The first user manual mentions that it is only needed to deal with that in the main folder,
but it is not the case: if you modify anything in Hera, in order to have the new version in the
GRiSP2 board, you need to remove that file everywhere. Note: “_rebar.lock_” is where the version
of dependencies are locked. This way our builds are reproducible, meaning if no code has been
changed in a repository, the build on the repository should always have the same result([https://github.com/erlang/rebar3/issues/2604](https://github.com/erlang/rebar3/issues/2604)). If you
forget to delete this and that you brought changes to the files, you will waste time building the
application, then testing it on the board only to realised it is still the old version.

&emsp;After any modifications to Hera, push every change to your git repositories. This has been automated using a bash script named “_push_hera.sh_”. This pushes the “_hera_” and “_hera_synchronization_”
folders, using your GitHub credentials. Modify it to put yours. Beware: put the bash script in
the “_.gitignore_” if you directly put your GitHub password there. However, there is a cleaner way
of doing it: instead of your password, use something like: “$ENVVAR” and set the environment
variable in your session using:
```bash
export ENVVAR=yourpassword
```
This only works _in your current shell session_, just like the rebar3 variable or the activation of the
Erlang installation. **Remember to always do the three commands upon starting a shell.**

&emsp;To use the script, in the main folder, type in the shell:
```bash
make COMMIT="Your amazing commit message" push_hera
```
If you want to create your own bash, feel free. But do not forget to type, in the shell:
```bash
chmod +x bashFileName.sh
```
to add execution rights.

&emsp;Next, make sure to delete both the “__build_” and “__grisp_” folders everywhere you find one.
Otherwise, it could have the same effect as the “_rebar.lock_” file. This can be done using a new
command of “_makefile_”:
```bash
make clear
```
You can finally build and deploy the application. To build, type:
```bash
rebar3 grisp build
```
Next, to deploy, the first thing to do is to format each SD-card as _fat32_. It is also suggested to
name it _GRISP_. The easiest way to achieve that is to use a partitioning tool like _KDE Partition
Manager_ or similar. You only need to do this once. Generally, the SD-card of your board should
already be formatted. Beware to adapt the _vm.args_ file to the name you give here.
&emsp;Now, plug the SD-card in your computer and use the _makefile_ again to deploy the software on
each SD-card with the command:
```bash
make deploy-hostname
```
where _hostname_ is the desired name of the GRiSP2 board.

### How to use the application

&emsp;Now that the SD-card is ready, you can plug it into the GRiSP2 board. If your board’s name is
starting by _nav_, make sure to also plug a PNAV in the SPI2 port of the GRiSP2. If the board’s
name starts by _order_, then no sensors are expected by the application. Also, make sure to have
your Wi-Fi hotspot up and ready.

#### The detector

&emsp; Let us suppose that the board’s name is “_nav_1_” (this was chosen for historical reasons, as the previous users of Hera would have many nodes with a PNAV
plugged-in and number them to distinguish the nodes. Here, as there is only one board with a PNAV, it is not really useful). After booting, the two LEDs should turn
red: this is because no calibration information was found in the system. Indeed, certain sensors
require a calibration in order to be used. Within the network, in case of restart, as long as there
is at least one node staying alive, the information will remain available.

&emsp;The gesture recognition algorithm uses the measurements from the **nav3.erl** module, which
requires calibration to correct the gyroscope and magnetometer defaults. However, since for now
the algorithm only uses the accelerometer data, no real calibration is required. Though, the system
requires calibration data. The procedure to calibrate a “_nav_” board is the following:
  - Connect serially to the Erlang shell using picocom;
  - In the shell, type:
```erlang
movement_detection:set_args(nav3).
```
If you wish to use another sensor or another measurement method than _nav3_, refer to the [previous user manual](https://github.com/lunelis/sensor_fusion) for more detail.
  - Text will appear in the shell. As it is not necessary to calibrate for this application, you can
press three times “Enter” to finish the calibration step.

&emsp; The LEDs are still showing red. They will only become green, indicating that there is calibration
information in the system, when the measurements are launched. To launch measurements, type next:

```erlang
movement_detection:launch().
```
Note: if your system comprises many nodes, you can launch every node at once using:
```erlang
movement_detection:launch_all().
```
Next, all one has to do is launch the gesture recognition algorithm. To do so, type:
```erlang
movement_detection:realtime(1.5,-1).
```
Using these arguments, the **grdos/12** function will launch and keep looping forever due to the
negative period. It will wait a minimum of 1.5 s between gestures to classify them, as explained
earlier. Now, you can move the board around to perform the predefined gestures.
&emsp; If you have trouble with getting the function started, check your Wi-Fi connection: the gesture
recognition algorithm does not work without a stable Wi-Fi connection.
&emsp; Note: if you want to test the effect of specific gestures without using the gesture recognition
algorithm, you can type the following two lines in the detector’s shell:
```erlang
net_adm:ping(movement_detection@orderCrate).
rpc:call(movement_detection@orderCrate,sendOrder,set_state_crate,[Name])
```
where **Name** is the movement whose effect you want to test. The first command will attempt
to connect the two boards. If it outputs **pang**, it means there is a network issue. If it outputs
**pong**, then the two boards managed to connect to each other. The second command then calls
the function **set_state_crate/1** from the sendOrder module on the receiver, with the **Name** as
argument.

#### The receiver

&emsp; Let us suppose the board’s name is “_orderCrate_”. There is nothing special to do here, beyond
booting the board. If it was named correctly, the two LEDs should light up, respectively in a light
blue and yellow colour, from left to right. Beware that it does not mean that the board connected
to the Wi-Fi. This is where having a hotspot is useful, because the user can see how many devices
are connected to it. In this application, two devices should be connected for everything to work:
the detector and the receiver.

#### What gestures can be performed in a successive manner?

&emsp; Every movement combinations are possible, however, some of them were deemed “unsafe” in the
present application and thus lead to a stop of the robot:
  - If one movement labelled as _forward_ is followed by one movement labelled as _backward_ or
vice-versa, the robot is stopped.
  - If the user wants to turn the robot around but did not stop prior to this, the robot is stopped.

#### Extensions to the platform and further development

&emsp; If you wish to add new dynamic measurements, other sensors or new sensor fusion models, the
author invites you to refer to the [previous user manual](https://github.com/lunelis/sensor_fusion), as nothing has changed since.














