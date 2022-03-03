FROM ubuntu:16.04

RUN apt-get update

RUN export PATH=$HOME/.local/bin:$PATH

RUN apt-get install -y build-essential erlang
RUN apt-get install -y git
RUN apt-get install -y graphviz
RUN apt-get install -y wget 

RUN wget https://theo.informatik.uni-rostock.de/storages/uni-rostock/Alle_IEF/Inf_THEO/images/tools_daten/lola-2.0.tar.gz
RUN tar xvzf lola-2.0.tar.gz

RUN cd lola-2.0 \
    && ./configure \
    && make \
    && make install

RUN apt install -y default-jre

# Uncomment to avoid caching repository, i.e. to force clone the newest version of the tool  
# RUN pwd
RUN git clone https://github.com/tamarit/pn_suite.git

RUN cd pn_suite \
    && make \
    && make install

WORKDIR /pn_suite
