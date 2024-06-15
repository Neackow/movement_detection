&emsp; NOTE: this README is the exact copy of Appendix E of the final thesis .pdf, which can be found by clicking [here](https://github.com/Neackow/movement_detection/blob/main/ISENGUERRE_50041800_2024.pdf). Do not hesitate to ask Pr. Peter Van Roy for the link to the official file from the dial.mem library. Also note that some files were deleted in this version of the project. To see the deleted files, refer to the previous theses: 
  - Lucas Nélis' version (numerl developper): [https://github.com/lunelis/sensor_fusion](https://github.com/lunelis/sensor_fusion) ;
  - Hera2.0: the original version, by Sébastien Kalbusch and Vincent Verpoten: [https://github.com/sebkm/sensor_fusion](https://github.com/sebkm/sensor_fusion) ;

&emsp; **BEWARE**: these versions are outdated when it comes to this thesis. All the necessary files are stored in this repository)

# Updated user manual

&emsp; In previous theses, the authors have created and updated a user manual detailing how to use their application, whilst also globally explaining how to start up with the GRiSP board and Erlang. However, it was never updated for the GRiSP2 board.

&emsp; The author encountered numerous problems, which were solved one by one over the course of several weeks. To avoid future generations of Erlang programmers struggling with the GRiSP environment, the author here proposes a guide on how to install Erlang, Hera, etc., followed by a user manual explaining how the application developed in this thesis can be used.

&emsp; Both of these are heavily inspired by the previous user manual, which can be found [here](https://github.com/lunelis/sensor_fusion). This user manual is also a great source of information concerning the older versions of the system.

## How to install Erlang, Hera

&emsp; Each part starts with the “go-to” procedure to complete the installation. An erratum and summary of problems faced at each step by the author can be found in the **_Struggles faced_** part of each section.

### ** Disclaimer **

&emsp; The following tutorial was done on a computer running on Linux-Ubuntu 20.04, installed in February 2024. The author cannot guarantee it will work for newer versions (though it should) or for other OS (E.g.: do not even try to use wsl on Windows. There are a lot of dependencies that do not exist in wsl but that are required to make this whole thing work. Linux is the go-to OS for the GRiSP environment.), nor does he guarantee that updated versions of the packages required to run Erlang and compile it will work with GRiSP. This tutorial, furthermore, is not as detailed as one would hope it to be: the installation process took place in early February, whilst the writing of this section was done in late May. If any questions emerge, the author will gladly help future users with their struggles. Note: another great source of help is the GRiSP [Slack channel](https://github.com/grisp/grisp/wiki).


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
version details where to find the files, which helps the process.):

``` erlang
{ plugins , [
  { rebar3_hex ,
    { git , " https :// github . com / erlef / rebar3_hex . git " ,
      { branch , " main "}
    }
  } ,
  { rebar3_grisp ,
    { git , " https :// github . com / grisp / rebar3_grisp . git " ,
      { branch , " master "}
  }
  }
]}.

```






