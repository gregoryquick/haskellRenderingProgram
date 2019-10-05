FROM ubuntu as builder
ENV TZ=Antarctica/Palmer
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup
RUN apt-get install -y pkg-config
RUN apt-get install -y gtk2.0
RUN apt-get install -y build-essential libgtk2.0-dev
RUN pkg-config gtk+-2.0 --libs
COPY . .
WORKDIR app
RUN stack build
CMD stack run