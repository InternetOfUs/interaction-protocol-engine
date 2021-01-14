# syntax=docker/dockerfile:experimental

FROM ubuntu:latest

RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y update

# Install java compiling
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install openjdk-11-jdk
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install maven

# Install swi prolog
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install software-properties-common
RUN apt-add-repository ppa:swi-prolog/stable
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y update
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install swi-prolog

# Install utilities
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install less

RUN echo "PS1='\[\033[01;32m\]WeNet@Interaction-Protocol-Engine-Dev\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \$ '" >> /root/.bashrc
RUN echo "alias mvnd=\"mvn -Dmaven.surefire.debug=\\\"-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=0.0.0.0:5005 -Xnoagent -Djava.compiler=NONE\\\"\"" >> /root/.bashrc

WORKDIR /app

ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64

CMD ["bash"]