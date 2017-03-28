FROM ubuntu
MAINTAINER glacier

RUN apt-get -qq update
RUN apt-get -y install sudo

RUN apt-get -y install software-properties-common python-software-properties
RUN add-apt-repository ppa:plt/racket
RUN apt-get update
RUN apt-get -y install racket

RUN apt-get -y install git



