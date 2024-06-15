&emsp; NOTE: this README is the exact copy of Appendix E of the final thesis .pdf, which can be found by clicking [here](https://github.com/Neackow/movement_detection/blob/main/ISENGUERRE_50041800_2024.pdf). Do not hesitate to ask Pr. Peter Van Roy for the link to the official file from the dial.mem library. Also note that some files were deleted in this version of the project. To see the deleted files, refer to the previous theses: 
  - Lucas Nélis' version (numerl developper): [https://github.com/lunelis/sensor_fusion](https://github.com/lunelis/sensor_fusion) ;
  - Hera2.0: the original version, by Sébastien Kalbusch and Vincent Verpoten: [https://github.com/sebkm/sensor_fusion](https://github.com/sebkm/sensor_fusion) ;

&emsp; **BEWARE**: these versions are outdated when it comes to this thesis. All the necessary files are stored in this repository)

# Updated user manual

&emsp; In previous theses, the authors have created and updated a user manual detailing how to use their application, whilst also globally explaining how to start up with the GRiSP board and Erlang. However, it was never updated for the GRiSP2 board.

&emsp; The author encountered numerous problems, which were solved one by one over the course of several weeks. To avoid future generations of Erlang programmers struggling with the GRiSP environment, the author here proposes a guide on how to install Erlang, Hera, etc., followed by a user manual explaining how the application developed in this thesis can be used.

&emsp; Both of these are heavily inspired by the previous user manual, which can be found [here](https://github.com/lunelis/sensor_fusion). This user manual is also a great source of information concerning the older versions of the system.

## How to install Erlang, Hera

&emsp; Each part starts with the “go-to” procedure to complete the installation. An erratum and summary of problems faced at each step by the author can be found in the _Struggles faced_ part of each section.

### ** Disclaimer **

&emsp; <div align="justify"> The following tutorial was done on a computer running on Linux-Ubuntu 20.04, installed in February 2024. The author cannot guarantee it will work for newer versions (though it should) or for other OS (E.g.: do not even try to use wsl on Windows. There are a lot of dependencies that do not exist in wsl but that are required to make this whole thing work. Linux is the go-to OS for the GRiSP environment.), nor does he guarantee that updated versions of the packages required to run Erlang and compile it will work with GRiSP. This tutorial, furthermore, is not as detailed as one would hope it to be: the installation process took place in early February, whilst the writing of this section was done in late May. If any questions emerge, the author will gladly help future users with their struggles. Note: another great source of help is the GRiSP [Slack channel](https://github.com/grisp/grisp/wiki). </div>

&emsp; Some parts of this tutorial are directly inspired by the GRiSP wiki. Refer to this [website](https://github.com/grisp/grisp/wiki) (GRiSP Wiki).


