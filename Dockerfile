FROM ubuntu:18.04

RUN apt-get update

RUN export PATH=$HOME/.local/bin:$PATH

RUN apt-get install -y build-essential erlang
RUN apt-get install -y git
RUN apt-get install -y graphviz
RUN apt-get install -y wget 

RUN wget https://theo.informatik.uni-rostock.de/storages/uni-rostock/Alle_IEF/Inf_THEO/images/tools_daten/lola-2.0.tar.gz
RUN tar xvzf lola-2.0.tar.gz



RUN apt install -y default-jre

# Uncomment to avoid caching repository, i.e. to force clone the newest version of the tool  
# RUN pwd
RUN git clone https://github.com/tamarit/pn_suite.git

RUN cd pn_suite \
    && make \
    && make install

RUN mv /pn_suite/examples /pn_suite/examples.bkp
RUN mv /pn_suite/src /pn_suite/src.bkp

RUN apt-get install -y python3
RUN apt-get install -y python3-pip
RUN python3 -m pip install snakes
RUN python3 -m pip install xmltodict
RUN python3 -m pip install networkx
RUN python3 -m pip install pydot
RUN python3 -m pip install cython
RUN python3 -m pip install numpy

# RUN python3 --version

# RUN cd lola-2.0 \
#     && ./configure \
#     && make \
#     && make install

WORKDIR /pn_suite
